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

import qualified Database.Persist as Ps
import qualified Database.Persist.Postgresql as PsP
import qualified Data.ByteString.Char8 as B
import           Database.Persist.TH
import           Database.Persist.Sql
import           Database.PostgreSQL.Simple.Time
import           Control.Monad.Logger (runNoLoggingT, NoLoggingT, runStderrLoggingT)
import Data.Time
import Services.Types
import Control.Monad.IO.Class
import Control.Monad.Trans.Resource
import Control.Monad.Trans.Reader (ReaderT)
import Helpers

conn = "host=localhost dbname=chess_test user=postgres"
inBack = inBackend conn

db = Database "test"

game1 = "e4 e5 Nf3 Nc6"
game2 = "a4 b5 ab5 a6"
games = [game1, game2]

attributes1 = [
    ("tournament", "London masters")
  , ("PlayerWhite", "Name, First")
  , ("PlayerBlack", "Name2, First2")] :: [(String, String)]
attributes2 = attributes1
attributes = [attributes1, attributes2]

moves1 = \game -> ImportantMove game 2 True "Bb5" "0.2" False True
moves2 = \game -> ImportantMove game 2 True "Nd4" "-3" True False
moves = [moves1, moves2]

class MonadIO m => HasPersist m where
  getPersistPool :: m ConnectionPool

runPersist :: HasPersist m => SqlPersistT (ResourceT (NoLoggingT IO)) b -> m b
runPersist action = do
  pool <- getPersistPool
  withPool pool action

-- | Run a database action
withPool :: MonadIO m
         => ConnectionPool
         -> SqlPersistT (ResourceT (NoLoggingT IO)) a -> m a
withPool cp f = liftIO . runNoLoggingT . runResourceT $ runSqlPool f cp


setupFake :: IO ()
setupFake = inBack $ do
  runMigration migrateAll
  dbResult <- Ps.insert db

  gamesResult <- mapM (Ps.insert . Game dbResult) games
  let atts = zip gamesResult attributes
  mapM_ (\(game, attList) -> mapM_ (\(name, val) -> Ps.insert (GameAttribute game name val)) attList) atts
  mapM_ (\game -> mapM_ (\mvFun -> Ps.insert (mvFun game)) moves) gamesResult
  return ()

main :: IO ()
main = do
  deleteDBContents conn
  setupFake
  return ()

-- Desired setup
-- run ./setup_fake to spin up `fake` database with fake data.
-- run app on 8001 and then run integration tests on the app
-- run unit tests and be sure that I can just continue with unit tests
