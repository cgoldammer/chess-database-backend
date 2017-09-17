{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE PolyKinds         #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}


module Services.Service where

import qualified Control.Lens as Lens
import Control.Monad.State.Class as SC
import Data.Aeson
import Snap.Core
import Snap.Snaplet
import Snap.Snaplet.PostgresqlSimple
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as BL
import Snap.Snaplet.Auth
import Snap.Snaplet.Persistent
import Control.Exception (try)
import qualified Data.Text as T
import Control.Monad
import Database.Esqueleto
import Data.Map as Map
import Servant.API hiding (GET, POST)
import qualified Database.Persist as Ps
import qualified Database.Persist.Postgresql as PsP
import Control.Monad.IO.Class (liftIO, MonadIO)
import Snap.Snaplet.Persistent
import Text.Read (readMaybe)
import Data.Maybe (isJust)
import Servant.API hiding (GET)
import Servant (serveSnap, Server, serveDirectory, ServerT)
import Data.Maybe
import Data.Proxy (Proxy(..))
import qualified Data.Time.Clock as C
import Services.Types


data Service = Service {
    _pg :: Snaplet Postgres
  , _db :: Snaplet PersistState}
Lens.makeLenses ''Service

instance HasPersistPool (Handler b Service) where
  getPersistPool = with db getPersistPool

instance HasPostgres (Handler b Service) where
  getPostgresState = with pg SC.get

chessCreateUser :: T.Text -> Handler b Service ()
chessCreateUser name = do
  time <- liftIO C.getCurrentTime
  -- Todo: Insert actual id
  let n = Just $ T.unpack name
  runPersist $ Ps.insert_ $ AppUser 1 n time
  return ()

type LoginUser = Maybe Int

chessApi :: Proxy (LevelApi (Handler b Service))
chessApi = Proxy

type LevelApi m = 
        "user" :> Get '[JSON] (Maybe AppUser)

currentUserName :: SnapletLens b (AuthManager b) -> Handler b Service (Maybe (T.Text))
currentUserName auth = do
    cu <- withTop auth $ do
      u <- currentUser
      return u
    return $ getCurrentUserName cu

getCurrentUserName :: Maybe AuthUser -> Maybe T.Text
getCurrentUserName a = fmap userLogin a

apiServer :: SnapletLens b (AuthManager b) -> Server (LevelApi (Handler b Service)) (Handler b Service)
apiServer auth = getMyUser
  where
    getMyUser = do
      user <- currentUserName auth
      users <- runPersist $ selectUser user
      return $ listToMaybe users

usId :: Maybe T.Text -> Int
usId x = maybe 0 (read . T.unpack) x

serviceInit :: SnapletLens b (AuthManager b) -> SnapletInit b Service
serviceInit auth = makeSnaplet "chess" "Chess Service" Nothing $ do
  pg <- nestSnaplet "pg" pg pgsInit
  d <- nestSnaplet "db" db $ initPersist (runMigrationUnsafe migrateAll)
  addRoutes $ chessRoutes auth
  return $ Service pg d

chessRoutes auth = [("", serveSnap chessApi (apiServer auth))]

getUser :: SnapletLens b (AuthManager b) -> Handler b Service ()
getUser auth = do
  user <- withTop auth $ do
    cur <- currentUser
    return cur
  let x = maybe 0 id $ fmap (read . T.unpack) (fmap unUid $ join $ fmap userId user) :: Int
  writeLBS . encode $ x

selectUser :: MonadIO m => Maybe T.Text -> SqlPersistT m [AppUser]
selectUser (Just name) = do
  users <- select $ from $ \user -> do
    let n = Just $ T.unpack name
    where_ $ user ^. AppUserName ==. val n
    return user
  return $ entityVal <$> users
selectUser Nothing = do
  return []

userFields = fmap B.pack ["name"]

postUser :: SnapletLens b (AuthManager b) -> Handler b Service ()
postUser auth = do
  user :: Int <- withTop auth $ do
    cur <- currentUser
    let x = fmap unUid $ join $ fmap userId cur
    return $ usId x
  [name] <- mapM getPostParam userFields
  usersFound  <- query "select id from user where id=?" [user]
  if length (usersFound :: [Only Int]) > 0
    then do
      return ()
    else do
      execute "INSERT INTO level_user (id) values (?)" [user]
      return ()

  [Only userId] :: [Only Int] <- query "select id from level_user where id=?" [user]
  when (isJust name) $ do
    execute "UPDATE level_user SET name=? WHERE id=?" (name, user)
    return ()
  return ()
