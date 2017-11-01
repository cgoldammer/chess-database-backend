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
import qualified Data.List as L
import qualified Data.Maybe as M
import Control.Monad.IO.Class
import Control.Monad.Trans.Resource
import Control.Monad.Trans.Reader (ReaderT)

import Services.Types
import Helpers

import qualified Chess.Pgn as Pgn

conn = "host=localhost dbname=chess_test user=postgres"

inBack :: ReaderT SqlBackend (NoLoggingT (ResourceT IO)) () -> IO ()
inBack = inBackend conn

main :: IO ()
main = do
  deleteDBContents conn
  readGamesIntoDB
  -- evaluateGames

-- queries to run:
-- strength of average move (show in elo equivalent)
-- show rating and performance by time period
-- show performance by move number, e.g. am I good in
-- opening or endgame?
-- how good is Anish Giri - should be amazing in opening.
-- do for certain time period.
  
files = ["game.pgn"]
numberOfGames = 1

readGamesIntoDB :: IO ()
readGamesIntoDB = mapM_ storeFileIntoDB files

storeFileIntoDB :: String -> IO ()
storeFileIntoDB fileName = inBack $ do
  dbResult <- Ps.insert (Database fileName)
  let fullName = "./test/files/" ++ fileName
  games :: [Pgn.PgnGame] <- liftIO $ Pgn.getGames fullName numberOfGames
  mapM_ (storeGameIntoDB dbResult) games
  return ()

storeGameIntoDB dbResult g = do
  let pgn = L.intercalate " " $ Pgn.pgnMoves g
  let tags = (Pgn.pgnGameTags g) :: [Pgn.PgnTag]
  players <- storePlayers tags
  let (playerWhite, playerBlack) = M.fromJust players

  -- Storing the game
  let gm = (Game dbResult playerWhite playerBlack pgn) :: Game
  gameResult <- Ps.insert gm
  -- Storing the tags
  let formattedTags = fmap formatForDB $ filter (not . filterPlayer) tags
  mapM_ (\(name, val) -> Ps.insert (GameAttribute gameResult name val)) formattedTags
  return ()

  
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
  

evaluateGames :: IO ()
evaluateGames = undefined

formatForDB :: Pgn.PgnTag -> (String, String)
formatForDB (Pgn.PgnEvent s) = ("Event", s)
formatForDB (Pgn.PgnOther name s) = (name, s)
formatForDB (Pgn.PgnDate s) = ("Date", s)
formatForDB (Pgn.PgnRound s) = ("Round", show s)
formatForDB (Pgn.PgnWhite player) = ("White", show player)
formatForDB (Pgn.PgnBlack player) = ("White", show player)

