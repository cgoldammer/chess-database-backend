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

conn = "host=localhost dbname=chess user=postgres"

db = Database "test"

game1 = "e4 e5 Nf3 Nc6"
game2 = "a4 b5 ab5 a6"
games = [game1, game2]

attributes1 = [("tournament", "London masters"), ("PlayerWhite", "Name, First")] :: [(String, String)]
attributes2 = [("tournament", "London masters"), ("PlayerWhite", "Name, First")] :: [(String, String)]
attributes = [attributes1, attributes2]

moves1 = \game -> ImportantMove game 2 True "Bb5" "0.2" False True
moves2 = \game -> ImportantMove game 2 True "Nd4" "-3" True False
moves = [moves1, moves2]

-- A `HasPersist` monad is simply a state monad that stores the pool


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

-- this is the repeated code that can be factored out
inBackend :: ReaderT SqlBackend (NoLoggingT (ResourceT IO)) a -> IO a
inBackend action = runStderrLoggingT $ PsP.withPostgresqlPool conn 10 $ \pool -> liftIO $ do
  flip runSqlPersistMPool pool $ do
    runMigration migrateAll
    action

deleteDBContents :: IO ()
deleteDBContents = inBackend $ do
  Ps.deleteWhere ([] :: [Ps.Filter ImportantMove])
  Ps.deleteWhere ([] :: [Ps.Filter GameAttribute])
  Ps.deleteWhere ([] :: [Ps.Filter Game])
  Ps.deleteWhere ([] :: [Ps.Filter Database])
  return ()

setupFake :: IO ()
setupFake = inBackend $ do
  runMigration migrateAll
  dbResult <- Ps.insert db
  gamesResult <- mapM (Ps.insert . Game dbResult) games
  let atts = zip gamesResult attributes
  mapM_ (\(game, attList) -> mapM_ (\(name, val) -> Ps.insert (GameAttribute game name val)) attList) atts
  let gameMoves = zip gamesResult moves
  mapM_ (\(game, mvFun) -> Ps.insert (mvFun game)) gameMoves
  return ()

main = do
  deleteDBContents
  setupFake
  return ()

-- Desired setup
-- run ./setup_fake to spin up `fake` database with fake data.
-- run app on 8001 and then run integration tests on the app
-- run unit tests and be sure that I can just continue with unit tests
