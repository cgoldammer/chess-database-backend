{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeFamilies               #-}

module Test.Helpers (
    inBackend
  , DataAction
  , formatForDB
  , deleteDBContents)
where

import Database.Persist (deleteWhere, Filter)
import Database.Persist.Postgresql as PsP (SqlBackend, withPostgresqlPool, runMigration, runSqlPersistMPool, runMigrationSilent)
import Control.Monad.Logger (NoLoggingT, runStderrLoggingT)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Resource (ResourceT)
import Control.Monad.Trans.Reader (ReaderT)
import qualified Data.ByteString.Char8 as B

import qualified Chess.Pgn.Logic as Pgn

import Services.Types

-- See https://stackoverflow.com/questions/34624469/crud-pattern-on-haskell-persistent
--
--
type DataAction a = ReaderT SqlBackend (NoLoggingT (ResourceT IO)) a

inBackend :: String -> DataAction a -> IO a
inBackend conn action = runStderrLoggingT $ withPostgresqlPool (B.pack conn) 1 $ \pool -> liftIO $ 
  flip runSqlPersistMPool pool $ do
    runMigration migrateAll
    runMigrationSilent migrateAll
    action

-- My goal: Use the `HasPersistPool` typeclass for all database actions. That
-- unifies the code I run in Snap and in the general IO Monad

deleteDBContents :: String -> IO ()
deleteDBContents conn = inBackend conn $ do
  deleteWhere ([] :: [Filter MoveEval])
  deleteWhere ([] :: [Filter GameAttribute])
  deleteWhere ([] :: [Filter PlayerRating])
  deleteWhere ([] :: [Filter Game])
  deleteWhere ([] :: [Filter Tournament])
  deleteWhere ([] :: [Filter Player])
  deleteWhere ([] :: [Filter DatabasePermission])
  deleteWhere ([] :: [Filter Database])
  return ()

formatForDB :: Pgn.PgnTag -> (String, String)
formatForDB (Pgn.PgnEvent s) = ("Event", s)
formatForDB (Pgn.PgnOther name s) = (name, s)
formatForDB (Pgn.PgnDate s) = ("Date", s)
formatForDB (Pgn.PgnSite s) = ("Site", s)
formatForDB (Pgn.PgnRound s) = ("Round", show s)
formatForDB (Pgn.PgnWhite player) = ("White", show player)
formatForDB (Pgn.PgnBlack player) = ("White", show player)
formatForDB (Pgn.PgnResult result) = ("White", show result)
formatForDB (Pgn.PgnWhiteElo rating) = ("WhiteElo", show rating)
formatForDB (Pgn.PgnBlackElo rating) = ("BlackElo", show rating)

