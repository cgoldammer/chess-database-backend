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
import Data.Either
import Control.Monad.IO.Class
import Control.Monad.Trans.Resource
import Control.Monad.Trans.Reader (ReaderT)
import Control.Monad.Reader.Class
import Control.Monad.Reader
import qualified Data.Either.Combinators as EitherC

import Services.Types
import Helpers
import Options.Applicative
-- import Data.Semigroup ((<>))

import qualified Chess.Pgn as Pgn
import qualified Chess.Board as Board
import qualified Chess.Stockfish as Stockfish

-- The connection string is obtained from the command line
-- Also, get settings for whether to create fake data.

connString :: String -> String
connString dbName = "host=localhost dbname=chess_" ++ dbName ++ " user=postgres"

data Settings = Settings { settingsDBName :: String, settingsIsDataTest :: Bool }

type IsTest = Bool

parse :: Parser IsTest
parse = switch (long "test" <> short 't' <> help "Run test analysis")

opts = info (parse <**> helper)
  ( fullDesc
  <> progDesc "Haskell-chess"
  <> header "" )

createSettings :: IsTest -> Settings
createSettings False = Settings "prod" False
createSettings True = Settings "test" True

getSettings :: IO Settings
getSettings = do
  isFake <- execParser opts
  return $ createSettings isFake

main :: IO ()
main = do
  settings <- getSettings
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

files = ["game.pgn"]
numberOfGames = 1

storeGamesIntoDB :: (MonadReader Settings m, MonadIO m) => m ()
storeGamesIntoDB = mapM_ storeFileIntoDB files

storeFileIntoDB :: (MonadReader Settings m, MonadIO m) => String -> m [Ps.Key Game]
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
  games <- liftIO $ inBackend (connString dbName) $ do
    dbGames :: [Entity Game] <- getGamesFromDB
    return dbGames
  liftIO $ print $ "Games:" ++ show (length games)
  evaluations :: [Key MoveEval] <- fmap concat $ mapM doEvaluation games
  return ()

evaluateGamesTest :: (MonadReader Settings m, MonadIO m) => m ()
evaluateGamesTest = do
  liftIO $ print "Test evaluation"
  return ()

doEvaluation :: (MonadReader Settings m, MonadIO m) => Entity Game -> m [Key MoveEval]
doEvaluation dbGame = do
  let game = dbGameToPGN $ entityVal $ dbGame
  summaries <- liftIO $ Pgn.gameSummaries game
  dbName <- reader settingsDBName
  keys <- liftIO $ inBackend (connString dbName) $ do
    k <- Ps.insertMany $ evalToRow (entityKey dbGame) summaries
    return k
  return keys



storeGameIntoDB dbResult g = do
  let pgn = Pgn.gamePgnFull $ Pgn.parsedPgnGame g
  let tags = (Pgn.pgnGameTags g) :: [Pgn.PgnTag]
  players <- storePlayers tags
  let (playerWhite, playerBlack) = M.fromJust players

  -- Storing the game
  let gm = (Game dbResult playerWhite playerBlack pgn) :: Game
  gameResult <- Ps.insert gm
  -- Storing the tags
  let formattedTags = fmap formatForDB $ filter (not . filterPlayer) tags
  mapM_ (\(name, val) -> Ps.insert (GameAttribute gameResult name val)) formattedTags
  return gameResult

  
filterPlayer :: Pgn.PgnTag -> Bool
filterPlayer (Pgn.PgnWhite _) = False
filterPlayer (Pgn.PgnBlack _) = False
filterPlayer _ = True

filterWhitePlayer :: Pgn.PgnTag -> Bool
filterWhitePlayer (Pgn.PgnWhite _) = True
filterWhitePlayer _ = False

filterBlackPlayer :: Pgn.PgnTag -> Bool
filterBlackPlayer (Pgn.PgnBlack _) = True
filterBlackPlayer _ = False

storePlayers :: [Pgn.PgnTag] -> DataResult (Maybe (Key Player, Key Player))
storePlayers tags = do
  let whitePlayer = M.listToMaybe $ filter filterWhitePlayer tags
  let blackPlayer = M.listToMaybe $ filter filterBlackPlayer tags
  res <- if (M.isJust whitePlayer && M.isJust blackPlayer)
    then do
    let (Pgn.PgnWhite (Pgn.Player firstWhite lastWhite)) = M.fromJust whitePlayer
    let (Pgn.PgnBlack (Pgn.Player firstBlack lastBlack)) = M.fromJust blackPlayer
    whiteResult <- Ps.insert (Player firstWhite lastWhite)
    blackResult <- Ps.insert (Player firstBlack lastBlack)
    return $ Just (whiteResult, blackResult)
  else do
    return Nothing
  return res
  
getGamesFromDB :: DataResult [Entity Game]
getGamesFromDB = do
  games <- Ps.selectList [] [LimitTo 1]
  return games

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

  
dbGameToPGN :: Game -> Pgn.Game
dbGameToPGN game = M.fromJust $ EitherC.rightToMaybe $ Pgn.pgnGame $ Pgn.unsafeMoves $ Te.pack $ gamePgn game


  

-- Questions I can ask
-- What was the average evaluation of Magnus' games by move number (moves 10, 20, 30)
-- compared to Giri
-- restrict to games between 2015 and 2017 and opponents >= 2700

-- averageEvalByMoveNumber :: Player -> TimeRange -> [(Int, Int)]
  
-- Controlling for own rating and opponent rating, what's the win and draw percentage
-- based on the computer evaluation?
