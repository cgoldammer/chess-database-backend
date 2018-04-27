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

module Services.DatabaseHelpers where

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
import qualified Data.Attoparsec.Text as Parsec
import qualified Turtle as Tu 
import Services.Types
import Test.Helpers as Helpers

import qualified Chess.Pgn.Logic as Pgn
import qualified Chess.Logic as Logic

import qualified Chess.Helpers as Helpers

import qualified Chess.Board as Board
import qualified Chess.Stockfish as Stockfish

connString :: String -> String
connString dbName = "host=localhost dbname=chess_" ++ dbName ++ " user=postgres"


keyReader = either entityKey id

storeGameIntoDB :: Key Database -> Pgn.PgnGame -> DataAction (Maybe (Key Game))
storeGameIntoDB dbResult g = do
  let pgn = Pgn.gamePgnFull $ Pgn.parsedPgnGame g
  let tags = (Pgn.pgnGameTags g) :: [Pgn.PgnTag]
  let requiredTags = trace (show tags) $ parseRequiredTags tags
  if isJust requiredTags 
    then do
      let parsedTags = fromJust requiredTags
      (playerWhite, playerBlack) <- storePlayers dbResult parsedTags
      tournament <- storeTournament dbResult parsedTags
      let resultInt = resultDBFormat $ requiredResult parsedTags
      let date = getDate tags -- Maybe Day
      -- Storing the game
      let gm = (Game dbResult playerWhite playerBlack resultInt tournament pgn date)
      gameResult <- fmap keyReader $ Ps.insertBy gm
      -- Storing the tags
      let formattedTags = fmap formatForDB $ filter (not . isPlayer) tags
      mapM_ (\(name, val) -> Ps.insert (GameAttribute gameResult name val)) formattedTags
      return $ Just gameResult
    else do
      return Nothing


storeTournament :: Key Database -> RequiredTags -> DataAction (Key Tournament)
storeTournament dbResult tags = do
  let (Pgn.PgnEvent eventName) = requiredEvent tags
  result <- Ps.insertBy $ Tournament dbResult eventName
  return $ keyReader result


storePlayers :: Key Database -> RequiredTags -> DataAction (Key Player, Key Player)
storePlayers dbResult tags = do
  let (whitePlayer, blackPlayer) = (requiredWhitePlayer tags, requiredBlackPlayer tags)
  let (Pgn.PgnWhite (Pgn.Player firstWhite lastWhite)) = whitePlayer
  let (Pgn.PgnBlack (Pgn.Player firstBlack lastBlack)) = blackPlayer
  whiteResult <- Ps.insertBy (Player dbResult firstWhite lastWhite)
  blackResult <- Ps.insertBy (Player dbResult firstBlack lastBlack)
  return (keyReader whiteResult, keyReader blackResult)


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
isPlayer _ = False

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

filterDate :: Pgn.PgnTag -> Bool
filterDate (Pgn.PgnDate _) = True
filterDate _ = False


resultDBFormat :: Pgn.PgnTag -> Int
resultDBFormat (Pgn.PgnResult Pgn.WhiteWin) = 1
resultDBFormat (Pgn.PgnResult Pgn.BlackWin) = -1
resultDBFormat (Pgn.PgnResult Pgn.Draw) = 0
resultDBFormat _ = 0

getDate :: [Pgn.PgnTag] -> Maybe Day
getDate tags = join $ fmap (\(Pgn.PgnDate d) -> EitherC.rightToMaybe (Parsec.parseOnly dateStringParse (Te.pack d))) $ listToMaybe $ filter filterDate tags

dateStringParse :: Parsec.Parser Day
dateStringParse = do
  year <- Parsec.many' Parsec.digit
  Parsec.char '.'
  month <- Parsec.many' Parsec.digit
  Parsec.char '.'
  day <- Parsec.many' Parsec.digit
  return $ fromGregorian (read year :: Integer) (read month :: Int) (read day :: Int)

readTextIntoDB :: MonadIO m => String -> String -> Te.Text -> Bool -> m (Ps.Key Database, [Maybe (Ps.Key Game)])
readTextIntoDB dbName chessDBName text isPublic = do
  res <- liftIO $ inBackend (connString dbName) $ readTextWithPersist chessDBName text isPublic
  return res

readTextWithPersist chessDBName text isPublic = do
  dbResult <- Ps.insert (Database chessDBName isPublic)
  let games = Pgn.getGamesFromText text
  gameResults <- mapM (storeGameIntoDB dbResult) $ rights games
  return (dbResult, gameResults)
