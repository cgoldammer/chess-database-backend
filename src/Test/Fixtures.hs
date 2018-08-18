{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module Test.Fixtures where

import Debug.Trace (traceShow)
import Database.Persist (Key, insertBy)
import Database.Persist.Sql 
import Data.Text as Te (Text, pack)
import Data.Either (rights)
import Control.Monad.Trans.Reader (ReaderT)
import Control.Monad (void)
import Control.Monad.Reader (MonadReader, MonadIO, runReaderT, reader, liftIO)
import Data.Either.Combinators (rightToMaybe)
import Text.RawString.QQ (r)
import qualified Filesystem.Path.CurrentOS as FS (fromText)
import qualified Turtle as Tu (strict, input)
import System.Directory (listDirectory)

import AppTypes
import Services.Types
import Test.Helpers as Helpers
import Services.DatabaseHelpers as DatabaseHelpers

import qualified Chess.Pgn.Logic as Pgn
import qualified Chess.Logic as Logic
import qualified Chess.Metrics as Metrics

import qualified Chess.Board as Board
import qualified Chess.Stockfish as Stockfish

-- The connection string is obtained from the command line
-- Also, get settings for whether to create fake data.

-- | The settings are obtained from the command line and determine
-- how the data is stored.
-- If the `settingsDelete` flag is set, all data is deleted from the database
-- before data is read in.
-- By default, data is not overwritten. If the program is stopped in the middle of inserting data
-- then running it again should simply continue the data insertion.
--
data FixtureSettings = FixtureSettings { 
    settingsDBName :: String
  , settingsRunEval :: Bool
  , settingsOnlyContinueEval :: Bool} deriving (Show)

type OnlyContinue = Bool

data SettingsInput = SettingsInput AppType OnlyContinue

doNothing :: IO ()
doNothing = return ()

runJob :: FixtureSettings -> IO ()
runJob settings = do
  let conn = connString $ settingsDBName settings
  let onlyContinueEval = settingsOnlyContinueEval settings
  if not onlyContinueEval then deleteDBContents conn else doNothing
  runReaderT readerActions settings
  return ()

doNothing' :: ReaderT FixtureSettings IO ()
doNothing' = return ()

readerActions :: ReaderT FixtureSettings IO ()
readerActions = do
  continue <- reader settingsOnlyContinueEval
  evaluate <- reader settingsRunEval
  if continue 
    then if evaluate then evaluateGames else doNothing'
    else do
      storeGamesIntoDB
      if evaluate then evaluateGames else doNothing'
  return ()

getFolderPgns :: String -> IO [String]
getFolderPgns folder = do
  allFiles :: [String] <- listDirectory $ "data/games/" ++ folder 
  let extension = reverse . take 4 . reverse
  let files = filter ((==".pgn") . extension) allFiles
  return [folder ++ "/" ++ name | name <- files]

fileSetsProd :: [(String, String)]
fileSetsProd = [
  ("World Championships 1886-2014", "prod/world_champion"), 
  ("Candidates 2011-2018", "prod/candidates"), 
  ("Wijk An Zee (Tata Steel) 2012-2018", "prod/wijk"),
  ("Rejkjavik Open 2018", "prod/rejkjavik"),
  ("Supertournaments 2017", "prod/super2017")]

parseSet :: String -> String -> IO (String, [String])
parseSet name folder = do
  files :: [String] <- getFolderPgns folder
  return (name, files)

getFiles :: AppType -> IO [(String, [String])]
getFiles Dev = return [("dummy games", ["dev/dummy_games.pgn"]), ("tata small", ["dev/tata_small.pgn"])]
getFiles Test = return [("dummy games", ["dev/dummy_games.pgn"])]
getFiles Prod = mapM (uncurry parseSet) fileSetsProd


storeGamesIntoDB :: (MonadReader FixtureSettings m, MonadIO m) => m ()
storeGamesIntoDB = do
  dbName <- reader settingsDBName
  files <- liftIO (getFiles (getAppType dbName))
  mapM_ storeFilesIntoDB files

storeFile :: MonadIO m => String -> String -> String -> m ()
storeFile dbName chessDBName fileName = do
  let fullName = "./data/games/" ++ fileName
  fileText <- Tu.strict $ Tu.input $ FS.fromText $ Te.pack fullName
  DatabaseHelpers.readTextIntoDB dbName chessDBName fileText True
  return ()

storeFilesIntoDB :: (MonadReader FixtureSettings m, MonadIO m) => (String, [String]) -> m ()
storeFilesIntoDB (chessDBName, fileNames) = do
  dbName <- reader settingsDBName
  liftIO $ inBackend (connString dbName) $ mapM_ (storeFile dbName chessDBName) fileNames
  return ()

evaluateGames :: (MonadReader FixtureSettings m, MonadIO m) => m ()
evaluateGames = do
  dbName <- reader settingsDBName
  continueEval <- reader settingsOnlyContinueEval
  games <- liftIO $ inBackend (connString dbName) $ do
    dbGames :: [Entity Game] <- getGamesFromDB continueEval
    return dbGames
  let gamesReversed = reverse games
  concat <$> mapM doEvaluation gamesReversed
  return ()


doEvaluation :: (MonadReader FixtureSettings m, MonadIO m) => Entity Game -> m [Key MoveEval]
doEvaluation dbGame = do
  dbName <- reader settingsDBName
  storeEvaluationIO dbName dbGame

type SummaryFunction = Int -> Logic.Game -> IO [Pgn.MoveSummary]

storeEvaluationIOHelper :: MonadIO m => SummaryFunction -> String -> Entity Game -> m [Key MoveEval]
storeEvaluationIOHelper summaryFunction dbName dbGame = do
  let maybeGame = dbGameToPGN $ entityVal dbGame
  let evalTime = 100
  case maybeGame of 
    (Just game) -> do
      summaries <- liftIO $ summaryFunction evalTime game
      liftIO $ inBackend (connString dbName) $ do
        k <- traceShow ("IO" ++ show summaries) $ mapM insertBy $ evalToRow (entityKey dbGame) summaries
        return $ rights k
    Nothing ->
      return []

storeEvaluationIO :: MonadIO m => String -> Entity Game -> m [Key MoveEval]
storeEvaluationIO = storeEvaluationIOHelper Pgn.gameSummaries

storeEvaluationIOFake :: MonadIO m => String -> Entity Game -> m [Key MoveEval]
storeEvaluationIOFake = storeEvaluationIOHelper Pgn.gameSummariesFake

-- | Adds structured player ratings to the database.
-- These ratings are already stored in raw format as part of the 
-- `game_tag` table. Here, we turn this raw data into monthly player
-- evaluations. 
-- The monthly evaluation is simply the average of the player's raw rating
-- over all games in a certain month. If a player has not played any games in 
-- a certain month, the `player_rating` table will not contain any data for this month.
-- If you are using this data to report player ratings graphs, you might
-- want to fill in this missing time period with the latest preceding rating.
ratingQuery :: Text
ratingQuery = [r|
SELECT player_id, extract(year from date) as year, extract(month from date) as month, avg(rating)::Int
FROM (
  SELECT player_black_id as player_id, date, CAST((COALESCE(value,'0')) AS INTEGER) as rating
  FROM game
  JOIN game_attribute ON game.id=game_attribute.game_id AND attribute='BlackElo' and value != ''
  UNION ALL
  SELECT player_white_id as player_id, date, CAST((COALESCE(value,'0')) AS INTEGER) as rating
  FROM game
  JOIN game_attribute ON game.id=game_attribute.game_id AND attribute='WhiteElo' and value != ''
) values
WHERE rating > 0 and date is not null
GROUP BY player_id, year, month
|]

type RatingQueryType = (Single Int, Single Int, Single Int, Single Int)

intToKey :: Int -> Key Player
intToKey = toSqlKey . fromIntegral

readRatingQuery :: RatingQueryType -> PlayerRating
readRatingQuery (Single player_id, Single year, Single month, Single rating) = PlayerRating (intToKey player_id) year month rating

addRatings :: DataAction ()
addRatings = do
  results :: [RatingQueryType] <- rawSql ratingQuery []
  mapM_ (insertBy . readRatingQuery) results
  return ()

sqlGamesAll :: Text
sqlGamesAll = [r|
SELECT ??
FROM game
|]

sqlGamesUnevaluated :: Text
sqlGamesUnevaluated = [r|
SELECT ?? 
FROM game
WHERE game.id not in (SELECT DISTINCT game_id from move_eval)
|]

getGamesFromDB :: Bool -> DataAction [Entity Game]
getGamesFromDB continueEval = do
  let query = if continueEval then sqlGamesUnevaluated else sqlGamesAll
  games :: [Entity Game] <- rawSql query []
  return games

evalToRow :: Key Game -> [Pgn.MoveSummary] -> [MoveEval]
evalToRow g ms = traceShow ("Move summary" ++ show ms) $ evalToRowColor g 1 Board.White ms

evalToRowColor :: Key Game -> Int -> Board.Color -> [Pgn.MoveSummary] -> [MoveEval]
evalToRowColor _ _ _ [] = []
evalToRowColor g n Board.White (ms : rest) = constructEvalMove g n True ms : evalToRowColor g n Board.Black rest
evalToRowColor g n Board.Black (ms : rest) = constructEvalMove g n False ms : evalToRowColor g (n + 1) Board.White rest

constructEvalMove :: Key Game -> Int -> Bool -> Pgn.MoveSummary -> MoveEval
constructEvalMove gm n isWhite (Pgn.MoveSummary mv mvBest evalMove evalBest fen) = MoveEval gm n isWhite (Just mv) mvBest eval evalB mate mateB fen
  where (eval, mate) = (evalInt evalMove, evalMate evalMove)
        (evalB, mateB) = (evalInt evalBest, evalMate evalBest)

evalInt :: Stockfish.Evaluation -> Maybe Int 
evalInt (Right n) = Just n
evalInt (Left _) = Nothing

evalMate :: Stockfish.Evaluation -> Maybe Int 
evalMate (Right _) = Nothing
evalMate (Left n) = Just n

dbGameToPGN :: Game -> Maybe Pgn.Game
dbGameToPGN game = rightToMaybe $ Logic.gameFromStart Pgn.pgnToMove $ Pgn.unsafeMoves $ Te.pack $ gamePgn game

toPosAttribute :: Key Position -> Metrics.StatType -> Int -> PositionAttribute
toPosAttribute pos stat = PositionAttribute pos (fromEnum stat)

gsToPosHelper :: Logic.GameState -> DataAction [PositionAttribute]
gsToPosHelper gs = do
  pos <- insertBy $ Position $ Logic.gameStateToFen gs
  return $ either (const []) (gsToPosAttributes gs) pos
        
gsToPosAttributes :: Logic.GameState -> Key Position -> [PositionAttribute]
gsToPosAttributes gs pos = attributes
  where attributes = fmap (uncurry (toPosAttribute pos)) stats
        stats = (Metrics.gameStateData . Metrics.getStats) gs

obtainGameAttributes :: Entity Game -> DataAction [[PositionAttribute]]
obtainGameAttributes dbGame = do
  let maybeGame = dbGameToPGN $ entityVal dbGame
  maybe (return []) (mapM gsToPosHelper . Logic.gameStates) maybeGame

storeGameAttributes :: Entity Game -> DataAction ()
storeGameAttributes dbGame = do
  attrs :: [[PositionAttribute]] <- obtainGameAttributes dbGame
  mapM_ insertBy $ concat attrs

storePositionAttribute :: PositionAttribute -> DataAction ()
storePositionAttribute pa = void $ insertBy pa

storeAtt :: Int -> DataAction ()
storeAtt num = do
  deleteWhere ([] :: [Filter PositionAttribute])
  deleteWhere ([] :: [Filter Position])
  games :: [Entity Game] <- selectList [] []
  let g = take num games
  mapM_ storeGameAttributes g

inb :: String -> DataAction a -> IO a
inb name = inBackend $ DatabaseHelpers.connString name
