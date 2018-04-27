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

import Database.PostgreSQL.Simple.Time
import Data.Time
import Control.Monad.IO.Class  (liftIO)
import Database.Persist
import Database.Persist.Postgresql
import Database.Persist.TH
import Data.Aeson
import Data.Aeson.Types


share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Database json
  name String
  isPublic Bool
  UniqueDatabaseName name

DatabasePermission json
  databaseId DatabaseId
  userId String
  write Bool
  read Bool
  admin Bool
  UniqueDatabasePermission databaseId userId

Tournament json
  databaseId DatabaseId
  name String
  UniqueTournamentName databaseId name

Game json
  databaseId DatabaseId
  playerWhiteId PlayerId
  playerBlackId PlayerId
  gameResult Int
  tournament TournamentId
  pgn String
  date Day Maybe
  UniqueGame databaseId playerWhiteId playerBlackId tournament pgn

GameAttribute json
  gameId GameId
  attribute String
  value String

MoveEval json
  gameId GameId
  moveNumber Int
  isWhite Bool
  movePlayed String Maybe
  moveBest String
  eval Int Maybe
  mate Int Maybe

Player json
  databaseId DatabaseId
  firstName String
  lastName String
  FullName databaseId firstName lastName

PlayerRating json
  playerId PlayerId
  year Int
  month Int
  rating Int
  UniqueRating playerId year month

AppUser json
  userId String
  name String Maybe
  subscriptionTime UTCTime default=CURRENT_TIMESTAMP
  deriving Show
|]

instance Show Database
  where show = show . toJSON

instance Show MoveEval
  where show = show . toJSON

instance Show Game
  where show = show . toJSON

instance Show Player where
  show p = playerFirstName p ++ " " ++ playerLastName p

