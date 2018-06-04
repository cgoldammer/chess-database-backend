{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}


module Services.Types where

import Data.Time (Day, UTCTime)
import Database.Persist.TH (persistLowerCase, share, mkPersist, sqlSettings, mkMigrate)
import Data.Aeson (toJSON)

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
  openingVariation OpeningVariationId Maybe
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
  fen String
  UniqueMoveEval gameId moveNumber isWhite

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
  subscriptionTime UTCTime
  deriving Show

OpeningCode json
  code String
  UniqueOpeningCode code

OpeningVariation json
  variationName String
  fen String
  standardMoves String
  code OpeningCodeId
  UniqueOpeningName fen
  
TestThing json
  name String
|]

instance Show Database
  where show = show . toJSON

instance Show MoveEval
  where show = show . toJSON

instance Show Game
  where show = show . toJSON

instance Show Player where
  show p = playerFirstName p ++ " " ++ playerLastName p

