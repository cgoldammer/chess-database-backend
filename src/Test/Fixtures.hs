{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module Test.Fixtures where

import Database.Persist (Key, insertBy)
import Database.Persist.Sql 
import Data.Text as Te (Text, pack)
import Data.Either (rights)
import Control.Monad.Trans.Reader (ReaderT)
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
doEvaluation dbGame  = do
  dbName <- reader settingsDBName
  doAndStoreEvaluationIO dbName dbGame


doAndStoreEvaluationIO :: MonadIO m => String -> Entity Game -> m [Key MoveEval]
doAndStoreEvaluationIO dbName dbGame = do
  let maybeGame = dbGameToPGN $ entityVal dbGame
  case maybeGame of 
    (Just game) -> do
      summaries <- liftIO $ Pgn.gameSummaries game
      liftIO $ inBackend (connString dbName) $ do
        k <- mapM insertBy $ evalToRow (entityKey dbGame) summaries
        return $ rights k
    Nothing ->
      return []

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
  SELECT player_black_id as player_id, date, value::Int as rating
  FROM game
  JOIN game_attribute ON game.id=game_attribute.game_id AND attribute='BlackPlayerElo'
  UNION ALL
  SELECT player_white_id as player_id, date, value::Int as rating
  FROM game
  JOIN game_attribute ON game.id=game_attribute.game_id AND attribute='WhitePlayerElo'
) values
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
evalToRow g = evalToRowColor g 1 Board.White

evalToRowColor :: Key Game -> Int -> Board.Color -> [Pgn.MoveSummary] -> [MoveEval]
evalToRowColor _ _ _ [] = []
evalToRowColor g n Board.White (ms : rest) = constructEvalMove g n True ms : evalToRowColor g n Board.Black rest
evalToRowColor g n Board.Black (ms : rest) = constructEvalMove g n False ms : evalToRowColor g (n + 1) Board.White rest

constructEvalMove :: Key Game -> Int -> Bool -> Pgn.MoveSummary -> MoveEval
constructEvalMove gm n isWhite (Pgn.MoveSummary mv mvBest evalMove _ _ fen) = MoveEval gm n isWhite (Just mv) mvBest eval mate fen
  where eval = evalInt evalMove
        mate = evalMate evalMove

evalInt :: Stockfish.Evaluation -> Maybe Int 
evalInt (Right n) = Just n
evalInt (Left _) = Nothing

evalMate :: Stockfish.Evaluation -> Maybe Int 
evalMate (Right _) = Nothing
evalMate (Left n) = Just n

dbGameToPGN :: Game -> Maybe Pgn.Game
dbGameToPGN game = rightToMaybe $ Logic.gameFromStart Pgn.pgnToMove $ Pgn.unsafeMoves $ Te.pack $ gamePgn game
