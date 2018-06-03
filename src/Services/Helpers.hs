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

import Prelude hiding (lookup)
import GHC.Generics (Generic)
import Data.Map (Map, mapKeys, assocs, lookup, elems, fromList, mapWithKey)
import Data.Aeson.Types (ToJSON, toJSON, fieldLabelModifier, genericToJSON, defaultOptions)
import Database.Persist.Postgresql (Key, entityKey, entityVal, Entity)
import Data.Char (toLower)
import Data.List (sortOn, groupBy)
import Control.Monad (join)
import Data.Maybe (fromJust)
import Control.Lens ((^.), _1, _2, _3, to)
import qualified Data.List.Safe as Safe (head)

import Services.StatsHelpers
import Services.Types

type EntityMap a = Map (Key a) a
type DataForMoveAverage = (GameResult, IsWhite, MoveEval)
data GameResult = Win | Draw | Lose
type EvalResult = (Entity MoveEval, Entity Game)

type MoveNumber = Int
type EvalInt = Int

-- |Taking a list of move evaluations and, if it has less then the desired number
-- of elements, padding it at the end. The padded value is the eveluation corresponding
-- to the game result.
padEvals :: Int -> GameResult -> [(MoveNumber, EvalInt)] -> [(MoveNumber, EvalInt)]
padEvals desiredLength result vals
  | length vals >= desiredLength = vals
  | otherwise = vals ++ zipped
      where zipped = zip [((length vals) + 1)..desiredLength] (repeat (resultValue result))

invertGameResult :: GameResult -> GameResult
invertGameResult Win = Lose
invertGameResult Lose = Win
invertGameResult Draw = Draw

getOwnGameResult :: GameResult -> IsWhite -> GameResult
getOwnGameResult gameResult True = gameResult
getOwnGameResult gameResult False = invertGameResult gameResult


moveAverage :: EntityMap Player -> Key Player -> [DataForMoveAverage] -> MoveSummary
moveAverage playerMap playerKey me = MoveSummary key playerName average
  where average = MoveAverage $ averageByPlayer me 
        player = fromJust $ lookup playerKey playerMap
        key = show playerKey
        playerName = show player


-- |Given the move evaluations, return the average evaluation for each player.
averageByPlayer :: [DataForMoveAverage] -> MoveAverageData
averageByPlayer dataForAverage = fmap calculateStats evals
  where mapByGame = groupWithVal (moveEvalGameId . (^. _3)) dataForAverage -- Map (Key Game) [DataForMoveAverage]
        maxLength = maximum $ fmap length $ elems mapByGame
        mapWithMove = fmap (aggregateEval maxLength) mapByGame -- Map (Key Game) [(Int, Int)]
        evals = (fmap . fmap) snd $ groupWithVal fst $ concat mapWithMove -- Map Int [Int]
        calculateStats x = (intAverage x, (stdError . fmap fromIntegral) x)

aggregateEval :: Int -> [DataForMoveAverage] -> [(Int, Int)]
aggregateEval maxLength dataForAverage = padEvals maxLength result $ zip moves evals
  where vals = fmap (\(_, isW, me) -> (isW, me)) dataForAverage -- [(IsWhite, MoveEval)]
        result = (head dataForAverage) ^. _1
        evals = fmap (uncurry evalAsIntWithColor) vals
        moves = fmap (^.(_2 . to moveEvalMoveNumber)) vals

entityToMap :: Ord (Key a) => [Entity a] -> Map (Key a) a
entityToMap ls = fromList $ [(entityKey x, entityVal x) | x <- ls]

maxEval :: Int
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

evalAsIntWithColor :: Bool -> MoveEval -> Int
evalAsIntWithColor True me = evalAsInt me
evalAsIntWithColor False me = - (evalAsInt me)

-- |Obtain a single number from a move evaluation, combining mates and non-mates.
-- The first step is to multiply all mates by a huge number, this ensures
-- that mates are always seen as better than non-mates. We then
-- top-code and bottom-code the results.
evalAsInt :: MoveEval -> Int
evalAsInt me = max (- maxEval) (min maxEval eval)
  where eval = fromJust singleNumber
        mateMultiplier = 100
        singleNumber = join $ Safe.head $ [moveEvalEval me, fmap (*mateMultiplier) (moveEvalMate me)]

groupWithVal :: (Ord b) => (a -> b) -> [a] -> Map b [a]
groupWithVal f x = fromList [(fst (head el), fmap snd el) | el <- grouped]
  where tuples = [(f val, val) | val <- sortOn f x]
        equal t t' = fst t == fst t'
        grouped = groupBy equal tuples -- [[(b, a)]]

addColor :: Key Player -> [EvalResult] -> [(GameResult, IsWhite, EvalResult)]
addColor player evalResults = [(ownGameResult g, isWhite g, evalResult) | evalResult@(_, g) <- evalResults]
  where isWhite g = if (gamePlayerWhiteId (entityVal g)) == player then True else False
        gameResult g = fromJust $ readGameResult $ gameGameResult $ entityVal g
        ownGameResult g = getOwnGameResult (gameResult g) (isWhite g)
        
playerBlack :: Entity MoveEval -> Entity Game -> Key Player
playerBlack _ gm = gamePlayerBlackId $ entityVal gm

playerNotToMove :: Entity MoveEval -> Entity Game -> Key Player
playerNotToMove me gm = if (moveEvalIsWhite m) then (gamePlayerBlackId g) else (gamePlayerWhiteId g)
  where m = entityVal me
        g = entityVal gm

-- Only keep the evaluations that have the other person to move
-- That provides the evaluation of the moves the player made.
movesByPlayer :: [EvalResult] -> Map (Key Player) [DataForMoveAverage]
movesByPlayer res = (fmap . fmap) keepRelevant groupedWithColor
  where grouped = groupWithVal (uncurry playerNotToMove) res -- Map (Key Player) [EvalResult]
        groupedWithColor = mapWithKey addColor grouped -- Map (Key Player) [DataForMoveAverage]
        keepRelevant (gr, isWhite, (me, _)) = (gr, isWhite, entityVal me)

data MoveSummary = MoveSummary {
  moveSummaryKey :: String
, moveSummaryPlayer :: String
, moveSummaryEvaluations :: MoveAverage } deriving (Generic)

instance ToJSON MoveSummary where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = cleanSummName } 

cleanSummName :: String -> String
cleanSummName s = [toLower c] ++ rest
  where (c: rest) = drop (length ("moveSummary" :: String)) s

type AvgMoveEval = Int
type AvgMoveStdError = Float

type MoveAverageData = Map MoveNumber (AvgMoveEval, AvgMoveStdError)
newtype MoveAverage = MoveAverage MoveAverageData

instance ToJSON MoveAverage where
  toJSON (MoveAverage ma) = toJSON $ mapKeys show ma


summarizeEvals :: [Entity Player] -> [EvalResult] -> [MoveSummary]
summarizeEvals players evals = fmap (handleMoveAverage playerMap) $ assocs byPlayer
  where byPlayer = movesByPlayer evals 
        playerMap = entityToMap players

handleMoveAverage :: Map (Key Player) Player -> (Key Player, [DataForMoveAverage]) -> MoveSummary
handleMoveAverage playerMap (playerKey, list) = moveAverage playerMap playerKey list

summarizeByPlayer :: a -> [EvalResult] -> [(Key Player, [DataForMoveAverage])]
summarizeByPlayer _ evals = list
  where list = assocs $ movesByPlayer evals 

type IsWhite = Bool

