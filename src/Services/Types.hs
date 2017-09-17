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


module Services.Types where

import           Snap.Snaplet.PostgresqlSimple
import           Database.PostgreSQL.Simple.Time
import Data.Time
import           Control.Monad.IO.Class  (liftIO)
import           Database.Persist
import           Database.Persist.Postgresql
import           Database.Persist.TH


share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Game json
  databaseId DatabaseId
  pgn String
Database json
  name String
  UniqueDatabaseName name
GameAttribute json
  gameId GameId
  attribute String
  value String
ImportantMove json
  gameId GameId
  moveNumber Int
  isWhite Bool
  move String
  eval String
  isBlunder Bool
  isBest Bool
AppUser json
  userId Int
  name String Maybe
  subscriptionTime UTCTime default=CURRENT_TIMESTAMP
  deriving Show
|]
