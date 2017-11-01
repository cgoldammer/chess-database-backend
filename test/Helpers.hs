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

module Helpers where

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

-- See https://stackoverflow.com/questions/34624469/crud-pattern-on-haskell-persistent
inBackend :: String -> ReaderT PsP.SqlBackend (NoLoggingT (ResourceT IO)) a -> IO a
inBackend conn action = runStderrLoggingT $ PsP.withPostgresqlPool (B.pack conn) 10 $ \pool -> liftIO $ do
  flip PsP.runSqlPersistMPool pool $ do
    PsP.runMigration migrateAll
    action

deleteDBContents :: String -> IO ()
deleteDBContents conn = inBackend conn $ do
  Ps.deleteWhere ([] :: [Ps.Filter MoveEval])
  Ps.deleteWhere ([] :: [Ps.Filter GameAttribute])
  Ps.deleteWhere ([] :: [Ps.Filter Game])
  Ps.deleteWhere ([] :: [Ps.Filter Player])
  Ps.deleteWhere ([] :: [Ps.Filter Database])
  return ()
