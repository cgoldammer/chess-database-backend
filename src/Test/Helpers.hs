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

module Test.Helpers (
    inBackend
  , DataAction
  , formatForDB
  , deleteDBContents)
where

import Services.Types
import qualified Database.Persist as Ps
import qualified Database.Persist.Postgresql as PsP
import           Control.Monad.Logger (runNoLoggingT, NoLoggingT, runStderrLoggingT)
import Data.Time
import Services.Types
import Control.Monad.IO.Class
import Control.Monad.Trans.Resource
import Control.Monad.Trans.Reader (ReaderT)
import qualified Data.ByteString.Char8 as B
import qualified Chess.Pgn.Logic as Pgn

-- See https://stackoverflow.com/questions/34624469/crud-pattern-on-haskell-persistent
--
--
type DataAction a = ReaderT PsP.SqlBackend (NoLoggingT (ResourceT IO)) a

inBackend :: String -> DataAction a -> IO a
inBackend conn action = runStderrLoggingT $ PsP.withPostgresqlPool (B.pack conn) 10 $ \pool -> liftIO $ do
  flip PsP.runSqlPersistMPool pool $ do
    PsP.runMigration migrateAll
    action

-- | Run a database action (taken from snaplet-persistent)
withPool :: MonadIO m
         => PsP.ConnectionPool
         -> PsP.SqlPersistM a -> m a
withPool cp f = liftIO . runResourceT . runNoLoggingT $ PsP.runSqlPool f cp

-- My goal: Use the `HasPersistPool` typeclass for all database actions. That
-- unifies the code I run in Snap and in the general IO Monad

deleteDBContents :: String -> IO ()
deleteDBContents conn = inBackend conn $ do
  Ps.deleteWhere ([] :: [Ps.Filter MoveEval])
  Ps.deleteWhere ([] :: [Ps.Filter GameAttribute])
  Ps.deleteWhere ([] :: [Ps.Filter PlayerRating])
  Ps.deleteWhere ([] :: [Ps.Filter Game])
  Ps.deleteWhere ([] :: [Ps.Filter Tournament])
  Ps.deleteWhere ([] :: [Ps.Filter Player])
  Ps.deleteWhere ([] :: [Ps.Filter Database])
  return ()

formatForDB :: Pgn.PgnTag -> (String, String)
formatForDB (Pgn.PgnEvent s) = ("Event", s)
formatForDB (Pgn.PgnOther name s) = (name, s)
formatForDB (Pgn.PgnDate s) = ("Date", s)
formatForDB (Pgn.PgnRound s) = ("Round", show s)
formatForDB (Pgn.PgnWhite player) = ("White", show player)
formatForDB (Pgn.PgnBlack player) = ("White", show player)
formatForDB (Pgn.PgnResult result) = ("White", show result)
formatForDB (Pgn.PgnWhiteElo rating) = ("WhiteElo", show rating)
formatForDB (Pgn.PgnBlackElo rating) = ("BlackElo", show rating)

