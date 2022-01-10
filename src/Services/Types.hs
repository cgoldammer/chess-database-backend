{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Services.Types where

import Data.Aeson (toJSON)
import Data.Time (Day, UTCTime)
import Database.Persist (Entity, entityKey)
import Database.Persist.TH (mkMigrate, mkPersist, persistLowerCase, share, sqlSettings)

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|

Database json
  name String
  isPublic Bool
  userId String Maybe
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

Position json
  fen String
  UniquePosition fen

PositionAttribute json
  positionId PositionId
  typ Int
  value Int
  UniquePositionAttribute positionId typ

MoveEval json
  gameId GameId
  moveNumber Int
  isWhite Bool
  movePlayed String Maybe
  moveBest String
  eval Int Maybe
  evalBest Int Maybe
  mate Int Maybe
  mateBest Int Maybe
  complexityGB Int Maybe
  fen String
  engineVersion String "default='SF 10'"
  created UTCTime
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

OpeningLine json
  name String
  UniqueOpeningLine name

OpeningVariation json
  variationName String
  fen String
  standardMoves String
  code OpeningCodeId
  line OpeningLineId
  UniqueOpeningName fen
  
TestThing json
  name String
|]

instance Eq AppUser
  where g == g' = appUserUserId g == appUserUserId g'

instance Show Database
  where show = show . toJSON

instance Show MoveEval
  where show = show . toJSON

instance Show Game
  where show = show . toJSON

instance Show Player where
  show p = playerFirstName p ++ " " ++ playerLastName p

instance {-# Overlaps #-} Eq (Entity Game)
  where g == g' = entityKey g == entityKey g'

instance {-# Overlaps #-} Ord (Entity Game) where
  g >= g' = entityKey g >= entityKey g'
  g <= g' = entityKey g <= entityKey g'
