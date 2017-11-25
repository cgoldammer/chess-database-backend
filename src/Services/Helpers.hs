{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE PolyKinds         #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module Services.Helpers where

import GHC.Generics (Generic)
import qualified Control.Lens as Lens

import qualified Data.Map as M
import Data.Aeson
import Data.Aeson.Types
import qualified Database.Persist as Ps
import qualified Database.Persist.Postgresql as PsP
import qualified Data.Char as C
import qualified Data.List as L
import qualified Data.Maybe as Mb

import Services.Types

type EntityMap a = M.Map (PsP.Key a) a
type DataForMoveAverage = (GameResult, IsWhite, MoveEval)
data GameResult = Win | Draw | Lose
type EvalResult = (PsP.Entity MoveEval, PsP.Entity Game)

padEvals :: Int -> GameResult -> [(Int, Int)] -> [(Int, Int)]
padEvals desiredLength result vals
  | length vals >= desiredLength = vals
  | otherwise = vals ++ zipped
      where zipped = zip [desiredLength..(length vals)] (repeat (resultValue result))

invertGameResult :: GameResult -> GameResult
invertGameResult Win = Lose
invertGameResult Lose = Win
invertGameResult Draw = Draw

getOwnGameResult :: GameResult -> IsWhite -> GameResult
getOwnGameResult gameResult True = gameResult
getOwnGameResult gameResult False = invertGameResult gameResult


intAverage :: [Int] -> Int
intAverage x = div (sum x) (length x)

moveAverage :: EntityMap Player -> Key Player -> [DataForMoveAverage] -> MoveSummary
moveAverage playerMap playerKey me = MoveSummary key playerName average
  where average = averageByPlayer me
        player = Mb.fromJust $ M.lookup playerKey playerMap
        key = show playerKey
        playerName = show player

-- Given the move evaluations, return the average evaluation for 
averageByPlayer :: [DataForMoveAverage] -> M.Map Int Int
averageByPlayer dataForAverage = fmap intAverage evals
  where mapByGame = groupWithVal (moveEvalGameId . (\(_, _, me) -> me)) dataForAverage -- Map (Key Game) [DataForMoveAverage]
        maxLength = maximum $ fmap length $ M.elems mapByGame
        mapWithMove = fmap (aggregateEval maxLength) mapByGame -- Map (Key Game) [(Int, Int)]
        evals = (fmap . fmap) snd $ groupWithVal fst $ concat mapWithMove -- Map Int [Int]

aggregateEval :: Int -> [DataForMoveAverage] -> [(Int, Int)]
aggregateEval maxLength dataForAverage = padEvals maxLength result $ zip moves evals
  where vals = fmap (\(_, isW, me) -> (isW, me)) dataForAverage -- [(IsWhite, MoveEval)]
        result = (\(r, _, _) -> r) $ head dataForAverage
        evals = fmap (uncurry evalAsIntWithColor) vals
        moves = fmap (\(_, me) -> moveEvalMoveNumber me) vals

entityToMap :: Ord (Key a) => [PsP.Entity a] -> M.Map (Key a) a
entityToMap ls = M.fromList $ [(PsP.entityKey x, PsP.entityVal x) | x <- ls]

maxEval = 400

resultValue :: GameResult -> Int
resultValue Win = maxEval
resultValue Lose = - maxEval
resultValue Draw = 0

readGameResult :: Int -> Maybe GameResult
readGameResult 1 = Just Win
readGameResult 0 = Just Draw
readGameResult (-1) = Just Lose
readGameResult _ = Nothing

evalAsIntWithColor True me = evalAsInt me
evalAsIntWithColor False me = - (evalAsInt me)

evalAsInt :: MoveEval -> Int
evalAsInt me = max (- maxEval) (min maxEval eval)
  where eval = evalHelper me

evalHelper (MoveEval _ _ _ _ _ (Just x) _) = x
evalHelper (MoveEval _ _ _ _ _ _ (Just x)) = x * 100

groupWithVal :: (Ord b, Eq b) => (a -> b) -> [a] -> M.Map b [a]
groupWithVal f x = M.fromList [(fst (head el), fmap snd el) | el <- grouped]
  where tuples = [(f val, val) | val <- L.sortOn f x]
        equal t t' = fst t == fst t'
        grouped = L.groupBy equal tuples -- [[(b, a)]]

addColor :: Key Player -> [EvalResult] -> [(GameResult, IsWhite, EvalResult)]
addColor player evalResults = [(ownGameResult g, isWhite g, evalResult) | evalResult@(er, g) <- evalResults]
  where isWhite g = if (gamePlayerWhiteId (PsP.entityVal g)) == player then True else False
        gameResult g = Mb.fromJust $ readGameResult $ gameGameResult $ PsP.entityVal g
        ownGameResult g = getOwnGameResult (gameResult g) (isWhite g)
        
playerBlack :: PsP.Entity MoveEval -> PsP.Entity Game -> Key Player
playerBlack _ gm = gamePlayerBlackId $ PsP.entityVal gm

playerNotToMove :: PsP.Entity MoveEval -> PsP.Entity Game -> Key Player
playerNotToMove me gm = if (moveEvalIsWhite m) then (gamePlayerBlackId g) else (gamePlayerWhiteId g)
  where m = PsP.entityVal me
        g = PsP.entityVal gm

-- Only keep the evaluations that have the other person to move
-- That provides the evaluation of the moves the player made.
movesByPlayer :: [EvalResult] -> M.Map (Key Player) [DataForMoveAverage]
movesByPlayer res = (fmap . fmap) keepRelevant groupedWithColor
  where grouped = groupWithVal (uncurry playerNotToMove) res -- Map (Key Player) [EvalResult]
        groupedWithColor = M.mapWithKey addColor grouped -- Map (Key Player) [DataForMoveAverage]
        keepRelevant (gr, isWhite, (me, _)) = (gr, isWhite, PsP.entityVal me)

data MoveSummary = MoveSummary {
    moveSummaryKey :: String
  , moveSummaryPlayer :: String
  , moveSummaryEvaluations :: MoveAverage } deriving (Generic)

instance ToJSON MoveSummary where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = cleanSummName } 

cleanSummName :: String -> String
cleanSummName s = [C.toLower c] ++ rest
  where (c: rest) = drop (length ("moveSummary" :: String)) s

type MoveAverage = M.Map Int Int

instance ToJSON MoveAverage where
  toJSON ma = toJSON $ M.mapKeys show ma


summarizeEvals :: [PsP.Entity Player] -> [EvalResult] -> [MoveSummary]
summarizeEvals players evals = fmap (handleMoveAverage playerMap) $ M.assocs byPlayer
  where byPlayer = movesByPlayer evals 
        playerMap = entityToMap players

handleMoveAverage :: M.Map (Key Player) Player -> (Key Player, [DataForMoveAverage]) -> MoveSummary
handleMoveAverage playerMap (playerKey, list) = moveAverage playerMap playerKey list

summarizeByPlayer players evals = list
  where list = M.assocs $ movesByPlayer evals 

type IsWhite = Bool

