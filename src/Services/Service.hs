{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
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
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE PolyKinds         #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module Services.Service where

import GHC.Generics (Generic)
import qualified Control.Lens as Lens
import Control.Monad.State.Class as SC
import Data.Aeson
import Data.Aeson.Types
import Snap.Core
import Snap.Snaplet
import Snap.Snaplet.PostgresqlSimple
import Data.Char as C
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as BL
import Snap.Snaplet.Auth
import Snap.Snaplet.Persistent
import Control.Exception (try)
import qualified Data.Text as T
import Control.Monad
import Servant.API hiding (GET, POST)

import qualified Database.Persist as Ps
import qualified Database.Persist.Postgresql as PsP
import Database.Esqueleto

import Control.Monad.IO.Class (liftIO, MonadIO)
import Snap.Snaplet.Persistent
import Text.Read (readMaybe)
import Data.Maybe (isJust)
import Servant.API hiding (GET)
import Servant (serveSnap, Server, serveDirectory, ServerT)
import Data.Maybe
import Data.Proxy (Proxy(..))
import qualified Data.Time.Clock as C
import qualified Data.Map as M
import Data.Conduit
import qualified Data.Conduit.List as CL
import Data.List as L
import Text.RawString.QQ
import Debug.Trace

import Services.Types
import qualified Services.Helpers as Helpers


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





currentUserName :: SnapletLens b (AuthManager b) -> Handler b Service (Maybe (T.Text))
currentUserName auth = do
    cu <- withTop auth $ do
      u <- currentUser
      return u
    return $ getCurrentUserName cu

getCurrentUserName :: Maybe AuthUser -> Maybe T.Text
getCurrentUserName a = fmap userLogin a

data ImpMove = ImpMove { impMove :: String, impBest :: String, impWhite :: String, impBlack :: String, impMoveEval :: String, impBestEval :: String } deriving (Generic, FromJSON, ToJSON)

-- The api returns a list of `ImpMove`s for each database (a string)
type ImportantResult = M.Map String [ImpMove] 

sql = [r|
SELECT 'test' as db, move, best, white, black, evalMove, evalBest
FROM game
JOIN (
  SELECT game_id, move as move, eval as evalMove 
  FROM important_move 
  WHERE is_blunder
) move 
ON game.id = move.game_id
JOIN (
  SELECT game_id, move as best, eval as evalBest from important_move 
  WHERE is_best
) moveBest
ON game.id = moveBest.game_id
JOIN (
  SELECT game_id, value as white 
  FROM game_attribute
  WHERE attribute='PlayerWhite'
  ) ga_white 
ON game.id=ga_white.game_id
JOIN (
  SELECT game_id, value as black 
  FROM game_attribute
  WHERE attribute='PlayerBlack'
  ) ga_black 
ON game.id=ga_black.game_id
|]

-- This is the required format (in JSON):
-- 	{'key': 2,
-- 	'player': 'Magnus Carlsen',
-- 	'evaluations': {1: 5, 2: -4, 5: 40}
-- 	}
--
-- selectUser :: MonadIO m => Maybe T.Text -> SqlPersistT m [AppUser]
-- selectUser (Just name) = do
--   users <- select $ from $ \user -> do
--     let n = Just $ T.unpack name
--     where_ $ user ^. AppUserName ==. val n
--     return user
--   return $ entityVal <$> users
-- selectUser Nothing = do
--   return []
--

test :: MonadIO m => SqlPersistT m [Entity AppUser]
test = do
  users <- select $ from $ \user -> do
    return user
  return users

getPlayers :: Handler b Service [Entity Player]
getPlayers = runPersist $ do
  players <- select $
    from $ \p -> do
      return p
  return players

getEvalResults :: Handler b Service [Helpers.EvalResult]
getEvalResults = do
  players :: [Entity Player] <- getPlayers
  let playerKeys = fmap entityKey players
  evals <- getMoveEvals playerKeys
  return evals

getMoveSummary :: Handler b Service [Helpers.MoveSummary]
getMoveSummary = do
  players :: [Entity Player] <- getPlayers
  let playerKeys = fmap entityKey players
  evals <- getMoveEvals playerKeys
  return $ Helpers.summarizeEvals players evals


selectEvalResults :: MonadIO m => [Key Player] -> SqlPersistT m [Helpers.EvalResult]
selectEvalResults players = do
  results <- select $ 
    from $ \(me, g) -> do
    where_ (me ^. MoveEvalGameId ==. g ^. GameId)
    return (me, g)
  return results

getMoveEvals :: [Key Player] -> Handler b Service [Helpers.EvalResult]
getMoveEvals players = do
  res <- runPersist $ selectEvalResults players
  return res

-- TODO
-- Print rows
-- Run tasks in shell

getCollections :: Handler b Service ImportantResult
getCollections = do
  res :: [ImpQueryResult] <- runPersist $ rawSql sql []
  let parsed = fmap constructMove res
  let comparer a b = fst a == fst b
  let dict = M.fromList $ zip (fmap fst parsed) $ (fmap . fmap) snd (L.groupBy comparer parsed)
  -- liftIO $ print dict
  -- writeLBS . encode dict
  return dict

printName :: GameAttributeId -> String
printName = show

gameRead :: Int -> String
gameRead ga = show ga

type ImpQueryResult = (Single String, Single String, Single String, Single String, Single String, Single String, Single String)
constructMove :: ImpQueryResult -> (String, ImpMove)
constructMove (Single db, Single mm, Single mb, Single mw, Single mblack, Single eb, Single em) = (db, ImpMove mm mb mw mblack eb em)

type T = (Single Int, Single Int)

useRes :: T -> String
useRes (Single x, _) = show x

queryTest :: Handler b Service ()
queryTest = do
  let sql = "SELECT game_id, (1 - game_id) FROM game_attribute"
  res :: [T] <- runPersist $ rawSql sql []
  liftIO $ print $ fmap useRes res
  writeLBS . encode $ fmap useRes res
  return ()


chessApi :: Proxy (ChessApi (Handler b Service))
chessApi = Proxy

type ChessApi m = 
       "user"     :> Get '[JSON] (Maybe AppUser) 
  :<|> "blunders" :> Get '[JSON] ImportantResult
  :<|> "moveSummary" :> Get '[JSON] [Helpers.MoveSummary]
  :<|> "players" :> Get '[JSON] [Player]
  :<|> "evalResults" :> Get '[JSON] [Helpers.EvalResult]

apiServer :: SnapletLens b (AuthManager b) -> Server (ChessApi (Handler b Service)) (Handler b Service)
apiServer auth = getMyUser :<|> getCollections :<|> getMoveSummary :<|> getPlayersOnly :<|> getEvalResults
  where
    getMyUser = do
      user <- currentUserName auth
      users <- runPersist $ selectUser user
      return $ listToMaybe users
    getPlayersOnly = (fmap . fmap) entityVal getPlayers

usId :: Maybe T.Text -> Int
usId x = maybe 0 (read . T.unpack) x

serviceInit :: SnapletLens b (AuthManager b) -> SnapletInit b Service
serviceInit auth = makeSnaplet "chess" "Chess Service" Nothing $ do
  pg <- nestSnaplet "pg" pg pgsInit
  d <- nestSnaplet "db" db $ initPersist (runMigrationUnsafe migrateAll)
  addRoutes $ chessRoutes auth
  return $ Service pg d

chessRoutes auth = [
    ("", serveSnap chessApi (apiServer auth))
  , ("test", queryTest)]

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
