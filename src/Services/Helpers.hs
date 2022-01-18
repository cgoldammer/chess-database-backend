{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Services.Helpers where

import Control.Lens ((^.), _1, _2, _3, to)
import Control.Monad (join)
import Data.Aeson.Types
  ( ToJSON
  , defaultOptions
  , fieldLabelModifier
  , genericToJSON
  , toJSON
  )
import Data.Char (toLower)
import Data.List (groupBy, sortOn)
import qualified Data.List.Safe as Safe (head)
import Data.Map (Map, assocs, elems, fromList, lookup, mapKeys, mapWithKey)
import Data.Maybe (catMaybes, fromJust, fromMaybe)
import Database.Persist (PersistEntity, PersistValue(PersistInt64), keyToValues)
import Database.Persist.Postgresql (Entity, Key, entityKey, entityVal, toSqlKey)
import GHC.Generics (Generic)
import Prelude hiding (lookup)

import Services.StatsHelpers
import Services.Types

type EntityMap a = Map (Key a) a

type DataForMoveAverage = (GameResult, IsWhite, MoveEval)

data GameResult
  = Win
  | Draw
  | Lose
  deriving (Eq, Show)

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
  where
    zipped = zip [(length vals + 1) .. desiredLength] (repeat (resultValue result))

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
        maxLength = maximum $ length <$> elems mapByGame
        mapWithMove = fmap (aggregateEval maxLength) mapByGame -- Map (Key Game) [(Int, Int)]
        evals = (fmap . fmap) snd $ groupWithVal fst $ concat mapWithMove -- Map Int [Int]
        calculateStats x = (intAverage x, (stdError . fmap fromIntegral) x)

aggregateEval :: Int -> [DataForMoveAverage] -> [(Int, Int)]
aggregateEval maxLength dataForAverage = maybe [] (\r -> padEvals maxLength r (zip moves evals)) result
  where vals = fmap (\(_, isW, me) -> (isW, me)) dataForAverage -- [(IsWhite, MoveEval)]
        result = (^. _1) <$> Safe.head dataForAverage
        evals = fmap (uncurry evalAsIntWithColor) vals
        moves = fmap (^.(_2 . to moveEvalMoveNumber)) vals

entityToMap :: Ord (Key a) => [Entity a] -> Map (Key a) a
entityToMap ls = fromList [(entityKey x, entityVal x) | x <- ls]

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

-- Todo: This should never happen, figure out why
errorEval :: Int
errorEval = 0

evalAsInt :: MoveEval -> Int
evalAsInt me = max (- maxEval) (min maxEval eval)
  where eval = fromMaybe errorEval singleNumber
        mateMultiplier = 100
        singleNumber = join $ Safe.head [moveEvalEval me, fmap (*mateMultiplier) (moveEvalMate me)]

-- Grouping a list based on a function on the list elements.
-- The `head` is safe here because it's only run on lists with at least one
-- element. This still feels hacky, but at least it's safe.
groupWithVal :: (Ord b) => (a -> b) -> [a] -> Map b [a]
groupWithVal f x = fromList [(fst (head el), fmap snd el) | el <- grouped, not (null el)]
  where tuples = [(f val, val) | val <- sortOn f x]
        equal t t' = fst t == fst t'
        grouped = groupBy equal tuples -- [[(b, a)]]

addColor :: Key Player -> [EvalResult] -> [(GameResult, IsWhite, EvalResult)]
addColor player evalResults = [(ownGameResult g, isWhite g, evalResult) | evalResult@(_, g) <- evalResults]
  where isWhite g = gamePlayerWhiteId (entityVal g) == player
        results g = readGameResult $ gameGameResult $ entityVal g
        gameResult g = fromJust $ results g
        ownGameResult g = getOwnGameResult (gameResult g) (isWhite g)
        
playerBlack :: Entity MoveEval -> Entity Game -> Key Player
playerBlack _ gm = gamePlayerBlackId $ entityVal gm

playerNotToMove :: Entity MoveEval -> Entity Game -> Key Player
playerNotToMove me gm = if moveEvalIsWhite m then gamePlayerBlackId g else gamePlayerWhiteId g
  where m = entityVal me
        g = entityVal gm

-- Only keep the evaluations that have the other person to move
-- That provides the evaluations of the moves the player made.
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
cleanSummName s = toLower c : rest
  where (c: rest) = drop (length ("moveSummary" :: String)) s

type AvgMoveEval = Int
type AvgMoveStdError = Float

type MoveAverageData = Map MoveNumber (AvgMoveEval, AvgMoveStdError)
newtype MoveAverage = MoveAverage MoveAverageData

instance ToJSON MoveAverage where
  toJSON (MoveAverage ma) = toJSON $ mapKeys show ma

summarizeEvals :: [Entity Player] -> [EvalResult] -> [MoveSummary]
summarizeEvals players evals = handleMoveAverage playerMap <$> assocs byPlayer
  where byPlayer = movesByPlayer evals 
        playerMap = entityToMap players

handleMoveAverage :: Map (Key Player) Player -> (Key Player, [DataForMoveAverage]) -> MoveSummary
handleMoveAverage playerMap (playerKey, list) = moveAverage playerMap playerKey list

summarizeByPlayer :: a -> [EvalResult] -> [(Key Player, [DataForMoveAverage])]
summarizeByPlayer _ evals = list
  where list = assocs $ movesByPlayer evals 

type IsWhite = Bool

dbKey :: PersistEntity a => Entity a -> Int
dbKey ent = dbKeyInt $ entityKey ent

dbKeyInt :: PersistEntity a => Key a -> Int
dbKeyInt key = head $ catMaybes $ keyInt <$> keyToValues key

keyInt :: PersistValue -> Maybe Int
keyInt (PersistInt64 a) = Just $ fromIntegral a
keyInt _ = Nothing

intToKey :: Int -> Key Tournament
intToKey = toSqlKey . fromIntegral

intToKeyDB :: Int -> Key Database
intToKeyDB = toSqlKey . fromIntegral

intToKeyGame :: Int -> Key Game
intToKeyGame = toSqlKey . fromIntegral

