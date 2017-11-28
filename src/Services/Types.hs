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
  UniqueDatabaseName name

Tournament json
  name String
  UniqueTournamentName name

Game json
  databaseId DatabaseId
  playerWhiteId PlayerId
  playerBlackId PlayerId
  gameResult Int
  tournament TournamentId
  pgn String
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
  firstName String
  lastName String
  FullName firstName lastName

AppUser json
  userId Int
  name String Maybe
  subscriptionTime UTCTime default=CURRENT_TIMESTAMP
  deriving Show
|]

instance Show MoveEval
  where show = show . toJSON

instance Show Game
  where show = show . toJSON

instance Show Player where
  show p = playerFirstName p ++ " " ++ playerLastName p

