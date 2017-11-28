{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ScopedTypeVariables        #-}

import qualified Database.Persist as Ps
import qualified Database.Persist.Postgresql as PsP
import qualified Data.ByteString.Char8 as B
import           Database.Persist.TH
import           Database.Persist.Sql
import           Database.PostgreSQL.Simple.Time
import           Control.Monad.Logger (runNoLoggingT, NoLoggingT, runStderrLoggingT)
import Data.Time
import qualified Data.Text as Te
import qualified Data.List as L
import qualified Data.Maybe as M
import Data.Maybe
import Data.Either
import Control.Monad.IO.Class
import Control.Applicative
import Control.Monad.Trans.Resource
import Control.Monad.Trans.Reader (ReaderT)
import Control.Monad.Reader.Class
import Control.Monad.Reader
import qualified Data.Either.Combinators as EitherC
import Debug.Trace
import Text.RawString.QQ

import Services.Types
import Helpers
import Options.Applicative
-- import Data.Semigroup ((<>))

import qualified Chess.Pgn.Logic as Pgn
import qualified Chess.Helpers as Helpers

import qualified Chess.Board as Board
import qualified Chess.Stockfish as Stockfish

-- The connection string is obtained from the command line
-- Also, get settings for whether to create fake data.

connString :: String -> String
connString dbName = "host=localhost dbname=chess_" ++ dbName ++ " user=postgres"

data Settings = Settings { 
    settingsDBName :: String
  , settingsIsDataTest :: Bool
  , settingsOnlyContinueEval :: Bool}

type IsTest = Bool
type OnlyContinue = Bool

data SettingsInput = SettingsInput IsTest OnlyContinue

dbTypeParser :: Parser String
dbTypeParser = fmap dbNameReader $ switch (long "test" <> short 't' <> help "Run test analysis")

dbNameReader :: IsTest -> String
dbNameReader False = "prod"
dbNameReader True = "test"

parse :: Parser Settings
parse = Settings
  <$> dbTypeParser
  <*> switch (long "runEval" <> short 'e' <> help "Run the evaluation")
  <*> switch (long "onlyContinueEval" <> short 'c' <> help "Only continue the evaluation")

opts = info (parse <**> helper)
  ( fullDesc
  <> progDesc "Haskell-chess"
  <> header "" )

dbForTest :: IsTest -> String
dbForTest True = "test"
dbForTest False = "prod"

main :: IO ()
main = do
  settings <- execParser opts
  runJob settings
  return ()

runJob :: Settings -> IO ()
runJob settings = do
  let conn = connString $ settingsDBName settings
  deleteDBContents conn
  runReaderT readerActions settings
  return ()

readerActions = do
  storeGamesIntoDB
  evaluateGames
  return ()

-- queries to run:
-- strength of average move (show in elo equivalent)
-- show rating and performance by time period
-- show performance by move number, e.g. am I good in
-- opening or endgame?
-- how good is Anish Giri - should be amazing in opening.
-- do for certain time period.
  
-- class DBIO m where
--   getInDB :: m (DataResult a -> IO a)

-- instance DBIO IO where
--   getInDB

files = ["tata.pgn"]
numberOfGames = 5



storeGamesIntoDB :: (MonadReader Settings m, MonadIO m) => m ()
storeGamesIntoDB = mapM_ storeFileIntoDB files

storeFileIntoDB :: (MonadReader Settings m, MonadIO m) => String -> m [Maybe (Ps.Key Game)]
storeFileIntoDB fileName = do
  dbName <- reader settingsDBName
  res <- liftIO $ inBackend (connString dbName) $ do
    dbResult <- Ps.insert (Database fileName)
    let fullName = "./test/files/" ++ fileName
    games :: [Pgn.ParsedGame] <- liftIO $ Pgn.getGames fullName numberOfGames
    gameResults <- mapM (storeGameIntoDB dbResult) $ rights games
    return gameResults
  return res

evaluateGames :: (MonadReader Settings m, MonadIO m) => m ()
evaluateGames = do
  isTest <- reader settingsIsDataTest
  case isTest of True -> evaluateGamesTest
                 False -> evaluateGamesReal
  return ()

evaluateGamesReal :: (MonadReader Settings m, MonadIO m) => m ()
evaluateGamesReal = do
  dbName <- reader settingsDBName
  continueEval <- reader settingsOnlyContinueEval
  games <- liftIO $ inBackend (connString dbName) $ do
    dbGames :: [Entity Game] <- getGamesFromDB continueEval
    return dbGames
  liftIO $ print $ "Games:" ++ show (length games)
  evaluations :: [Key MoveEval] <- fmap concat $ mapM doEvaluation games
  return ()

evaluateGamesTest :: (MonadReader Settings m, MonadIO m) => m ()
evaluateGamesTest = do
  liftIO $ print "Test evaluation"
  evaluateGamesReal
  return ()

doEvaluation :: (MonadReader Settings m, MonadIO m) => Entity Game -> m [Key MoveEval]
doEvaluation dbGame = do
  let maybeGame = dbGameToPGN $ entityVal $ dbGame
  keys <- case maybeGame of 
    (Just game) -> do
      summaries <- liftIO $ Pgn.gameSummaries game
      dbName <- reader settingsDBName
      keys <- liftIO $ inBackend (connString dbName) $ do
        k <- Ps.insertMany $ evalToRow (entityKey dbGame) summaries
        return k
      return keys
    Nothing ->
      return []
  return keys

resultDBFormat :: Pgn.PgnTag -> Int
resultDBFormat (Pgn.PgnResult Pgn.WhiteWin) = 1
resultDBFormat (Pgn.PgnResult Pgn.BlackWin) = -1
resultDBFormat (Pgn.PgnResult Pgn.Draw) = 0
resultDBFormat _ = 0

storeGameIntoDB :: Key Database -> Pgn.PgnGame -> DataResult (Maybe (Key Game))
storeGameIntoDB dbResult g = do
  let pgn = Pgn.gamePgnFull $ Pgn.parsedPgnGame g
  let tags = (Pgn.pgnGameTags g) :: [Pgn.PgnTag]

  let requiredTags = trace (show tags) $ parseRequiredTags tags
  if trace (show tags) $ isJust requiredTags then do
    let parsedTags = fromJust requiredTags
    (playerWhite, playerBlack) <- storePlayers parsedTags
    tournament <- storeTournament parsedTags
    let resultInt = resultDBFormat $ requiredResult parsedTags
    -- Storing the game
    let gm = (Game dbResult playerWhite playerBlack resultInt tournament pgn) :: Game
    gameResult <- fmap keyReader $ Ps.insertBy gm
    -- Storing the tags
    let formattedTags = fmap formatForDB $ filter (not . isPlayer) tags
    mapM_ (\(name, val) -> Ps.insert (GameAttribute gameResult name val)) formattedTags
    return $ Just gameResult
  else do
    return Nothing

data RequiredTags = RequiredTags {
    requiredWhitePlayer :: Pgn.PgnTag
  , requiredBlackPlayer :: Pgn.PgnTag
  , requiredResult :: Pgn.PgnTag
  , requiredEvent :: Pgn.PgnTag}

parseRequiredTags :: [Pgn.PgnTag] -> Maybe RequiredTags
parseRequiredTags tags = RequiredTags <$> maybeWhite <*> maybeBlack <*> maybeResult <*> maybeEvent
  where maybeWhite = Helpers.safeHead $ filter filterWhitePlayer tags
        maybeBlack = Helpers.safeHead $ filter filterBlackPlayer tags
        maybeResult = Helpers.safeHead $ filter filterResult tags
        maybeEvent = Helpers.safeHead $ filter filterEvent tags
  
isPlayer :: Pgn.PgnTag -> Bool
isPlayer (Pgn.PgnWhite _) = True
isPlayer (Pgn.PgnBlack _) = True
isPlayer _ = True

filterWhitePlayer :: Pgn.PgnTag -> Bool
filterWhitePlayer (Pgn.PgnWhite _) = True
filterWhitePlayer _ = False

filterBlackPlayer :: Pgn.PgnTag -> Bool
filterBlackPlayer (Pgn.PgnBlack _) = True
filterBlackPlayer _ = False

filterResult :: Pgn.PgnTag -> Bool
filterResult (Pgn.PgnResult _) = True
filterResult _ = False

filterEvent :: Pgn.PgnTag -> Bool
filterEvent (Pgn.PgnEvent _) = True
filterEvent _ = False

keyReader = either entityKey id

storePlayers :: RequiredTags -> DataResult (Key Player, Key Player)
storePlayers tags = do
  let (whitePlayer, blackPlayer) = (requiredWhitePlayer tags, requiredBlackPlayer tags)
  let (Pgn.PgnWhite (Pgn.Player firstWhite lastWhite)) = whitePlayer
  let (Pgn.PgnBlack (Pgn.Player firstBlack lastBlack)) = blackPlayer
  whiteResult <- Ps.insertBy (Player firstWhite lastWhite)
  blackResult <- Ps.insertBy (Player firstBlack lastBlack)
  return (keyReader whiteResult, keyReader blackResult)

storeTournament :: RequiredTags -> DataResult (Key Tournament)
storeTournament tags = do
  let (Pgn.PgnEvent eventName) = requiredEvent tags
  result <- Ps.insertBy $ Tournament eventName
  return $ keyReader result

-- select where the game id cannot be found in move_eval

sqlGamesAll = [r|
SELECT ??
|]

sqlGamesUnevaluated = [r|
SELECT ?? 
WHERE game.id not in (SELECT DISTINCT game_id from move_eval)
|]


getGamesFromDB :: Bool -> DataResult [Entity Game]
getGamesFromDB = undefined
-- getGamesFromDB continueEval = do
--   let query = if continueEval then sqlGamesUnevaluated else sqlGamesAll
--   games :: [Entity Game] <- rawSql query []
--   games <- Ps.selectList [] [LimitTo numberOfGames]
--   return games

evalToRow :: Key Game -> [Pgn.MoveSummary] -> [MoveEval]
evalToRow g ms = evalToRowColor g 1 Board.White ms

evalToRowColor :: Key Game -> Int -> Board.Color -> [Pgn.MoveSummary] -> [MoveEval]
evalToRowColor _ _ _ [] = []
evalToRowColor g n (Board.White) (ms : rest) = constructEvalMove g n True ms : evalToRowColor g n (Board.Black) rest
evalToRowColor g n (Board.Black) (ms : rest) = constructEvalMove g n False ms : evalToRowColor g (n + 1) (Board.White) rest

constructEvalMove :: Key Game -> Int -> Bool -> Pgn.MoveSummary -> MoveEval
constructEvalMove gm n isWhite (Pgn.MoveSummary mv mvBest evalMove evalBest _) = MoveEval gm n isWhite mvString mvBestString (evalInt evalMove) (evalMate evalMove)
  where mvString = Just $ Board.shortMove mv
        mvBestString = Board.shortMove mvBest

evalInt :: Stockfish.Evaluation -> Maybe Int 
evalInt (Right n) = Just n
evalInt (Left _) = Nothing

evalMate :: Stockfish.Evaluation -> Maybe Int 
evalMate (Right _) = Nothing
evalMate (Left n) = Just n

  
dbGameToPGN :: Game -> Maybe Pgn.Game
dbGameToPGN game = EitherC.rightToMaybe $ Pgn.pgnGame $ Pgn.unsafeMoves $ Te.pack $ gamePgn game


  

-- Questions I can ask
-- What was the average evaluation of Magnus' games by move number (moves 10, 20, 30)
-- compared to Giri
-- restrict to games between 2015 and 2017 and opponents >= 2700

-- averageEvalByMoveNumber :: Player -> TimeRange -> [(Int, Int)]
  
-- Controlling for own rating and opponent rating, what's the win and draw percentage
-- based on the computer evaluation?
