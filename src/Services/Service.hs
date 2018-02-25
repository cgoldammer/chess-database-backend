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
import GHC.Int
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
import Data.List
import Snap.Snaplet.Persistent
import Control.Exception (try)
import qualified Data.Text as T
import Control.Monad
import Language.Javascript.JQuery

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
import           Data.IORef

import           Control.Monad.Logger (runNoLoggingT, NoLoggingT, runStderrLoggingT)
import qualified Database.Persist.Postgresql as PG
import Data.Configurator as DC
import Services.Types
import qualified Services.Helpers as Helpers
import qualified Control.Monad.State.Lazy as St

type UserState = St.State (Maybe String) (Maybe String)

user :: St.State (Maybe String) (Maybe String)
user = do n <- St.get
          return n

data Service = Service {
    _pg :: Snaplet Postgres
  , _db :: Snaplet PersistState
  , _currentUser :: IORef (Maybe String)}
Lens.makeLenses ''Service

instance HasPersistPool (Handler b Service) where
  getPersistPool = with db getPersistPool

instance HasPostgres (Handler b Service) where
  getPostgresState = with pg SC.get

type LoginUser = Maybe Int

mkSnapletPgPoolWithDB :: (MonadIO (m b v), MonadSnaplet m) => String -> m b v ConnectionPool
mkSnapletPgPoolWithDB dbName = do
    conf <- getSnapletUserConfig
    maybeSize <- liftIO $ DC.lookup conf "postgre-pool-size"
    let conStr = B.pack $ "host='localhost' dbname='chess_" ++ dbName ++ "' user='postgres'"
    let size = maybe 1 id maybeSize
    liftIO . runNoLoggingT $ PG.createPostgresqlPool conStr size

initPersistWithDB :: String -> SqlPersistT (NoLoggingT IO) a -> SnapletInit b PersistState
initPersistWithDB dbName = initPersistGeneric $ mkSnapletPgPoolWithDB dbName

currentUserName :: Handler b Service (Maybe String)
currentUserName = do
  nameRef <- gets _currentUser
  user <- liftIO $ readIORef nameRef
  return user

-- currentUserName :: SnapletLens b (AuthManager b) -> Handler b Service (Maybe (T.Text))
-- currentUserName auth = do
--     cu <- withTop auth $ do
--       u <- currentUser
--       return u
--     return $ fmap userLogin cu

-- getCurrentUserName :: Maybe AuthUser -> Maybe T.Text
-- getCurrentUserName = fmap userLogin a

data ImpMove = ImpMove { impMove :: String, impBest :: String, impWhite :: String, impBlack :: String, impMoveEval :: String, impBestEval :: String } deriving (Generic, FromJSON, ToJSON)


test :: MonadIO m => SqlPersistT m [Entity AppUser]
test = do
  users <- select $ from $ \user -> do
    return user
  return users

changeUser :: Maybe String -> Handler b Service ()
changeUser val = do
  nameRef <- gets _currentUser
  liftIO $ writeIORef nameRef val
  return ()


getPlayers :: DefaultSearchData -> Handler b Service [Entity Player]
getPlayers searchData = runPersist $ do
  let db = val $ intToKeyDB $ searchDB searchData
  players <- select $ distinct $
    from $ \(p, g) -> do
      where_ $ ((g^.GamePlayerWhiteId ==. p^.PlayerId) ||. (g^.GamePlayerBlackId ==. p^.PlayerId)) &&. (g^.GameDatabaseId ==. db)
      return p
  return players

getDatabases :: Handler b Service [Entity Database]
getDatabases = runPersist $ do
  databases <- select $
    from $ \db -> do
      return db
  return databases

getTournaments :: DefaultSearchData -> Handler b Service [Entity Tournament]
getTournaments searchData = runPersist $ do
  let db = val $ intToKeyDB $ searchDB searchData
  tournaments <- select $ distinct $ 
    from $ \(t, g) -> do
      where_ $ (g^.GameDatabaseId ==. db) &&. (t^.TournamentId ==. g^.GameTournament)
      return t
  return tournaments

intToKey :: Int -> Key Tournament
intToKey = toSqlKey . fromIntegral

intToKeyDB :: Int -> Key Database
intToKeyDB = toSqlKey . fromIntegral


data DataSummary = DataSummary { 
    numberTournaments :: Int
  , numberGames :: Int
  , numberGameEvals :: Int
  , numberMoveEvals :: Int
} deriving (Generic, Show, Eq, ToJSON, FromJSON)

dataSummaryQuery = [r|
WITH 
    numberTournaments as (SELECT count(distinct id) as "numberTournaments" FROM tournament )
  , numberGames as (SELECT count(distinct id) as "numberGames" FROM game)
  , numberGameEvals as (
      SELECT count(distinct id) as "numberGameEvals" FROM (
          SELECT id FROM game WHERE id in (
            SELECT DISTINCT game_id FROM move_eval
          )
      ) as numberGameEvals
    )
  , numberMoveEvals as (SELECT count(*) as "numberMoveEvals" FROM move_eval)
SELECT * 
FROM numberTournaments
CROSS JOIN numberGames
CROSS JOIN numberGameEvals
CROSS JOIN numberMoveEvals
|]

type QueryType = (Single Int, Single Int, Single Int, Single Int)

getDataSummary :: DefaultSearchData -> Handler b Service DataSummary
getDataSummary searchData = do
  let db = searchDB searchData
  results :: [QueryType] <- runPersist $ rawSql dataSummaryQuery []
  let (Single numberTournaments, Single numberGames, Single numberGameEvals, Single numberMoveEvals) = head results
  return $ DataSummary numberTournaments numberGames numberGameEvals numberMoveEvals

evalData :: MoveRequestData -> Handler b Service ([Entity Player], [Helpers.EvalResult])
evalData mrData = do
  let db = moveRequestDB mrData
  let tournaments = moveRequestTournaments mrData
  let tournamentKeys = fmap intToKey tournaments
  players :: [Entity Player] <- getPlayers $ DefaultSearchData db
  let playerKeys = fmap entityKey players
  evals <- getMoveEvals (intToKeyDB db) playerKeys tournamentKeys
  return (players, evals)

getEvalResults :: MoveRequestData -> Handler b Service [Helpers.EvalResult]
getEvalResults mrData = do
  (_, evals) <- evalData mrData
  return evals

getMoveSummary :: MoveRequestData -> Handler b Service [Helpers.MoveSummary]
getMoveSummary mrData = do
  (playerKeys, evals) <- evalData mrData
  return $ Helpers.summarizeEvals playerKeys evals

selectEvalResults :: MonadIO m => Key Database -> [Key Player] -> [Key Tournament] -> SqlPersistT m [Helpers.EvalResult]
selectEvalResults db players tournaments = do
  results <- select $ 
    from $ \(me, g, t) -> do
    where_ $ (me ^. MoveEvalGameId ==. g ^. GameId) &&. (g ^. GameTournament ==. t ^. TournamentId) &&. (t ^. TournamentId `in_` valList tournaments) &&. (g^.GameDatabaseId ==. val db)
    return (me, g)
  return results

getMoveEvals :: Key Database -> [Key Player] -> [Key Tournament] -> Handler b Service [Helpers.EvalResult]
getMoveEvals db players tournaments = runPersist (selectEvalResults db players tournaments)

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

data MoveRequestData = MoveRequestData {moveRequestDB :: Int, moveRequestTournaments :: [Int] } deriving (Generic, FromJSON, ToJSON, Show)

data ResultPercentage = ResultPercentage {
    ownElo :: Int
  , opponentElo :: Int
  , winPercentage :: Int
  , drawPercentage :: Int} deriving (Generic, FromJSON, ToJSON, Show)

data DefaultSearchData = DefaultSearchData { searchDB :: Int } deriving (Generic, FromJSON, ToJSON, Show)

type ChessApi m = 
       "user"     :> Get '[JSON] (Maybe AppUser) 
  :<|> "test" :> Get '[JSON] Int
  :<|> "players" :> ReqBody '[JSON] DefaultSearchData :> Post '[JSON] [Entity Player]
  :<|> "tournaments" :> ReqBody '[JSON] DefaultSearchData :> Post '[JSON] [Entity Tournament]
  :<|> "databases" :> Get '[JSON] [Entity Database]
  :<|> "evalResults" :> ReqBody '[JSON] MoveRequestData :> Post '[JSON] [Helpers.EvalResult]
  :<|> "moveSummary" :> ReqBody '[JSON] MoveRequestData :> Post '[JSON] [Helpers.MoveSummary]
  :<|> "dataSummary" :> ReqBody '[JSON] DefaultSearchData :> Post '[JSON] DataSummary
  :<|> "resultPercentages" :> ReqBody '[JSON] DefaultSearchData :> Post '[JSON] [ResultPercentage]
  :<|> "games" :> ReqBody '[JSON] GameRequestData :> Post '[JSON] [GameDataFormatted]

data GameRequestData = GameRequestData {
    gameRequestDB :: Int
  , gameRequestTournaments :: [Int]
} deriving (Generic, FromJSON, ToJSON)

apiServer :: Server (ChessApi (Handler b Service)) (Handler b Service)
apiServer =      getMyUser 
            :<|> getTest
            :<|> getPlayers
            :<|> getTournaments 
            :<|> getDatabases 
            :<|> getEvalResults 
            :<|> getMoveSummary 
            :<|> getDataSummary
            :<|> getResultPercentages
            :<|> getGames
  where
    getTest = do
      return 1
    getMyUser = do
      user <- currentUserName 
      users <- runPersist $ selectUser $ fmap T.pack user
      return $ listToMaybe users

type ResultPercentageQueryResult = (Single Int, Single Int, Single Int, Single Int)

toResultPercentage :: ResultPercentageQueryResult -> ResultPercentage
toResultPercentage (Single ownElo, Single opponentElo, Single winPercentage, Single drawPercentage) = ResultPercentage ownElo opponentElo winPercentage drawPercentage

resultPercentageQuery = [r|
SELECT rating_own
    , rating_opponent
    , (100 * avg((result=1)::Int))::Int as share_win
    , (100 * avg((result=0)::Int)) :: Int as share_draw
FROM (
  SELECT game_result as result
      , 100 * floor(rating1.rating/100) as rating_own
      , 100 * floor(rating2.rating/100) as rating_opponent
      , eval, move_number
  FROM game
  JOIN player_rating as rating1 ON 
        game.player_white_id=rating1.player_id
    AND extract(year from game.date)=rating1.year
    AND extract(month from game.date)=rating1.month
  JOIN player_rating as rating2 ON 
        game.player_black_id=rating2.player_id
    AND extract(year from game.date)=rating2.year
    AND extract(month from game.date)=rating2.month
  JOIN move_eval on game.id=move_eval.game_id
  WHERE is_white AND move_number>0 and game.database_id=?
) values
GROUP BY rating_own, rating_opponent
|]

type GameData = (Entity Game, Entity Tournament, Entity Player, Entity Player, Entity GameAttribute)

data GameDataFormatted = GameDataFormatted {
    gameDataGame :: Entity Game
  , gameDataTournament :: Entity Tournament
  , gameDataPlayerWhite :: Entity Player
  , gameDataPlayerBlack :: Entity Player
  , gameDataAttributes :: [Entity GameAttribute]} deriving (Generic, FromJSON, ToJSON)

getGames :: GameRequestData -> Handler b Service [GameDataFormatted]
getGames requestData = fmap gameGrouper $ runPersist $ getGames' requestData

groupSplitter :: [GameData] -> GameDataFormatted
groupSplitter ((g, t, pWhite, pBlack, ga) : rest) = GameDataFormatted g t pWhite pBlack allAttributes
  where allAttributes = ga : (fmap (\(_, _, _, _, ga) -> ga) rest)

gameDataEqual :: GameData -> GameData -> Bool
gameDataEqual (g, _, _, _, _) (g', _, _, _, _) = entityKey g == entityKey g'

gameGrouper :: [GameData] -> [GameDataFormatted]
gameGrouper allGames = fmap groupSplitter $ Data.List.groupBy gameDataEqual allGames

getGames' :: MonadIO m => GameRequestData -> SqlPersistT m [GameData]
getGames' requestData = do
  let db = intToKeyDB $ gameRequestDB requestData
  let tournaments = gameRequestTournaments requestData
  let tournamentKeys = fmap intToKey tournaments
  let tournamentMatch t = t ^. TournamentId `in_` valList tournamentKeys
  results <- select $ 
    from $ \(g, t, pWhite, pBlack, ga) -> do
      where_ $ 
            (g ^. GameTournament ==. t ^. TournamentId) 
        &&. (g ^. GameDatabaseId ==. val db)
        &&. (if length tournaments > 0 then (tournamentMatch t) else (not_ (tournamentMatch t)))
        &&. (pWhite ^. PlayerId ==. g ^. GamePlayerWhiteId) 
        &&. (pBlack ^. PlayerId ==. g ^. GamePlayerBlackId) 
        &&. (ga ^. GameAttributeGameId ==. g ^. GameId)
      return (g, t, pWhite, pBlack, ga)
  return results

getResultPercentages :: DefaultSearchData -> Handler b Service [ResultPercentage]
getResultPercentages searchData = do
  let db = searchDB searchData
  results :: [ResultPercentageQueryResult] <- runPersist $ rawSql resultPercentageQuery [PersistInt64 (fromIntegral db)]
  return $ fmap toResultPercentage results

usId :: Maybe T.Text -> Int
usId x = maybe 0 (read . T.unpack) x

serviceInit :: String -> SnapletInit b Service
serviceInit dbName = makeSnaplet "chess" "Chess Service" Nothing $ do
  pg <- nestSnaplet "pg" pg pgsInit
  d <- nestSnaplet "db" db $ initPersistWithDB dbName (runMigrationUnsafe migrateAll)
  addRoutes chessRoutes
  user <- liftIO $ newIORef $ Just "Test IT"
  return $ Service pg d user

chessRoutes = [("user2", writeBS "user test")] ++ [("", serveSnap chessApi apiServer)]

-- getUser :: SnapletLens b (AuthManager b) -> Handler b Service ()
-- getUser auth = do
--   cur <- currentUserName
--   let x = maybe 0 id $ fmap (read . T.unpack) (fmap unUid $ join $ fmap userId user) :: Int
--   writeLBS . encode $ x

-- chessCreateUser :: T.Text -> Handler b Service ()
-- chessCreateUser name = do
--   time <- liftIO C.getCurrentTime
--   -- Todo: Insert actual id
--   let n = Just $ T.unpack name
--   runPersist $ Ps.insert_ $ AppUser 1 n time
--   return ()

createAppUser :: T.Text -> Handler b Service ()
createAppUser userLogin = do
  time <- liftIO C.getCurrentTime
  trace ("Creating user" ++ show userLogin) $ runPersist $ Ps.insert_ $ AppUser (T.unpack userLogin) Nothing time
  return ()

selectUser :: MonadIO m => Maybe T.Text -> SqlPersistT m [AppUser]
selectUser (Just usId) = do
  users <- select $ from $ \user -> do
    let idString = T.unpack usId :: String
    -- let idKey = (toSqlKey . fromIntegral) idString
    where_ $ user ^. AppUserUserId ==. val idString
    return user
  return $ entityVal <$> users
selectUser Nothing = do
  return []

userFields = fmap B.pack ["name"]

-- postUser :: SnapletLens b (AuthManager b) -> Handler b Service ()
-- postUser = do
--   user :: Int <- withTop auth $ do
--     cur <- currentUser
--     let x = fmap unUid $ join $ fmap userId cur
--     return $ usId x
--   [name] <- mapM getPostParam userFields
--   usersFound  <- query "select id from user where id=?" [user]
--   if length (usersFound :: [Only Int]) > 0
--     then do
--       return ()
--     else do
--       execute "INSERT INTO level_user (id) values (?)" [user]
--       return ()

--   [Only userId] :: [Only Int] <- query "select id from level_user where id=?" [user]
--   when (isJust name) $ do
--     execute "UPDATE level_user SET name=? WHERE id=?" (name, user)
--     return ()
--   return ()
