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
import qualified Data.Text.Lazy as TL
import Snap.Snaplet.Auth
import Data.List
import Control.Exception (try)
import qualified Data.Text as T
import qualified Data.Text.Template as Template
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
import qualified Services.DatabaseHelpers as DatabaseHelpers
import qualified Control.Monad.State.Lazy as St
import qualified Test.Fixtures as TF
import qualified Test.Helpers as TH

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
getDatabases = do
  currentUser :: Maybe String <- currentUserName
  dbs <- runPersist $ do
    dbsPublic <- select $ from $ \db -> do
      where_ (db^.DatabaseIsPublic)
      return db
    let searchUser = maybe "NOUSER" id currentUser
    let searchCondition db dbp = (dbp^.DatabasePermissionUserId ==. (val searchUser)) &&. (dbp^.DatabasePermissionRead ==. (val True))
    let mergeCondition db dbp = dbp^.DatabasePermissionDatabaseId ==. db^.DatabaseId
    dbsPersonal <- select $ distinct $ 
      from $ \(db, dbp) -> do
        where_ $ (mergeCondition db dbp) &&. (searchCondition db dbp)
        return db
    return $ dbsPublic ++ dbsPersonal
  return dbs
  


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
    numberTournaments as (SELECT count(distinct id) as "numberTournaments" FROM tournament where database_id=?)
  , numberGames as (SELECT count(distinct id) as "numberGames" FROM game where database_id=?)
  , numberGameEvals as (
      SELECT count(distinct id) as "numberGameEvals" FROM (
          SELECT id FROM game WHERE id in (
            SELECT DISTINCT game_id FROM move_eval
          ) and game.database_id=?
      ) as numberGameEvals
    )
  , numberMoveEvals as (
      SELECT count(*) as "numberMoveEvals" 
      FROM move_eval
      JOIN game on move_eval.game_id = game.id
      WHERE game.database_id=?
    )
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
  let arguments = take 4 $ repeat $ PersistInt64 (fromIntegral db)
  results :: [QueryType] <- runPersist $ rawSql dataSummaryQuery arguments
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
  writeLBS . encode $ fmap useRes res
  return ()

data MoveRequestData = MoveRequestData {moveRequestDB :: Int, moveRequestTournaments :: [Int] } deriving (Generic, FromJSON, ToJSON, Show)

data ResultPercentage = ResultPercentage {
    ownElo :: Int
  , opponentElo :: Int
  , winPercentage :: Int
  , drawPercentage :: Int} deriving (Generic, FromJSON, ToJSON, Show)

data DefaultSearchData = DefaultSearchData { searchDB :: Int } deriving (Generic, FromJSON, ToJSON, Show)


type GameList = [Int]
type GameEvaluation = Int
type Performance = Int
type ResultByEvaluation = [(Int, [(GameEvaluation, Performance)])]
type TT = M.Map Int [GameEvaluation] -- deriving (Generic, ToJSON)


parseEvalResults :: (Single Int, Single Int, Single Int, Single Int) -> (Int, GameEvaluation, Performance)
parseEvalResults (_, Single playerId, Single evaluation, Single result) = (playerId, evaluation, result)


toList :: (Single Int, Single Int) -> Int
toList (Single a, _) = a

listToInClause :: [Int] -> String
listToInClause ints = clause
  where intStrings = (fmap show ints) :: [String]
        clause = "(" ++ (intercalate ", " intStrings) ++ ")"










-- The evaluation of a move is eval - lag(eval, 1) by game. Top-code at 0.
-- then average by game and color.
-- After that, merge onto the game table and obtain the performance by each player.

viewQuery :: T.Text
viewQuery = [r|
CREATE OR REPLACE VIEW moveevals as (
  SELECT game_id, is_white, move_number, greatest(eval - next_eval, 0) as cploss FROM (
    SELECT game_id, is_white, move_number, eval, lead(eval) over (partition by game_id order by id) as next_eval
    FROM move_eval) as lags);
|]

evalQueryTemplate :: T.Text
evalQueryTemplate = [r|
  WITH me_player as (
    SELECT g.id as game_id, player_white_id, player_black_id, is_white, cploss, game_result
    FROM moveevals me
    JOIN game g
    ON me.game_id = g.id
    WHERE g.id in $gameList
  )
  SELECT game_id, player_white_id as player_id, avg(cploss)::Int as cploss, avg((game_result+1)/2*100)::Int as result from me_player WHERE is_white group by game_id, player_white_id
  UNION ALL
  SELECT game_id, player_black_id as player_id, avg(cploss)::Int as cploss, avg((-game_result+1)/2*100)::Int as result from me_player WHERE not is_white group by game_id, player_black_id;
|]

testQueryTemplate = [r| SELECT id, id from game where id in $c|]

testQueryString :: [Int] -> T.Text
testQueryString ints = TL.toStrict $ Template.substitute testQueryTemplate cont
  where cont = context [("c", listToInClause ints)]

evalQueryString :: [Int] -> T.Text
evalQueryString ints = TL.toStrict $ Template.substitute evalQueryTemplate cont
  where cont = context [("gameList", listToInClause ints)]

-- | Create 'Context' from association list.
context :: [(String, String)] -> Template.Context
context assocs x = T.pack $ maybe err id . L.lookup (T.unpack x) $ assocs
  where err = error $ "Could not find key: " ++ T.unpack x

testQuery :: Handler b Service [Int]
testQuery = do
  let vals = take 100 $ [0..100]
  results <- trace (T.unpack (testQueryString vals)) $ runPersist $ rawSql (testQueryString vals) []
  return $ fmap toList $ results

getResultByEvaluation :: GameList -> Handler b Service ResultByEvaluation
getResultByEvaluation gl = do
  runPersist $ rawExecute viewQuery []
  results <- runPersist $ rawSql (evalQueryString gl) []
  let parsed = fmap parseEvalResults results
  let grouped = (fmap . fmap) (\(_, b, c) -> (b, c)) $ Helpers.groupWithVal (\(a, _, _) -> a) parsed
  return $ M.toList grouped



data UploadData = UploadData { uploadName :: String, uploadText :: T.Text } deriving (Generic, FromJSON)
data UploadResult = UploadResult (Maybe Int) deriving (Generic, ToJSON)

data EvaluationRequest = EvaluationRequest { evaluationDB :: Int, evaluationOverwrite :: Bool } deriving (Generic, FromJSON)
type EvaluationResult = Int


addEvaluations :: EvaluationRequest -> Handler b Service EvaluationResult
addEvaluations request = do
  let dbKey = intToKeyDB $ evaluationDB request
  let overwrite = evaluationOverwrite request
  dbName <- getDBName
  games :: [Entity Game] <- liftIO $ gamesInDB dbName dbKey overwrite
  evaluations <- liftIO $ do
    evals :: [[Key MoveEval]] <- sequenceA $ fmap (TF.doAndStoreEvaluationIO dbName) games
    return evals
  return $ length $ concat evaluations

gamesInDB :: String -> Key Database -> Bool -> IO [Entity Game]
gamesInDB dbName dbKey overwrite = TH.inBackend (DatabaseHelpers.connString dbName) $ do
  let db = val dbKey
  evaluatedGames :: [Entity Game] <- select $ distinct $
    from $ \(g, me) -> do
      where_ $ (me ^. MoveEvalGameId ==. g^.GameId) &&. (g^.GameDatabaseId ==. db)
      return g
  allGames :: [Entity Game] <- select $ distinct $ 
    from $ \g  -> do
      where_ (g^.GameDatabaseId ==. db)
      return g
  let evaluatedIds = (fmap entityKey evaluatedGames) :: [Key Game]
  let difference = [g | g <- allGames, not (entityKey g `elem` evaluatedIds)]
  return $ if overwrite then allGames else difference


getDBName :: Handler b Service String
getDBName = do
  conf <- getSnapletUserConfig
  dbNameMaybe :: Maybe String <- liftIO $ DC.lookup conf "dbName"
  let dbName = maybe "dev" id dbNameMaybe
  return dbName


uploadDB :: UploadData -> Handler b Service UploadResult
uploadDB upload = do
  let (name, text) = (uploadName upload, uploadText upload)
  dbName <- getDBName
  (db, results) <- liftIO $ DatabaseHelpers.readTextIntoDB dbName name text False
  currentUser :: Maybe String <- currentUserName
  addDBPermission db currentUser
  return $ UploadResult $ Just $ length results

addDBPermission :: Key Database -> Maybe String -> Handler b Service (Key DatabasePermission)
addDBPermission dbResult userName = do
  permission <- runPersist $ Ps.insert $ DatabasePermission dbResult (maybe "" id userName) True True False
  return permission

data GameRequestData = GameRequestData {
    gameRequestDB :: Int
  , gameRequestTournaments :: [Int]
} deriving (Generic, FromJSON, ToJSON)

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
  :<|> "uploadDB" :> ReqBody '[JSON] UploadData :> Post '[JSON] UploadResult
  :<|> "addEvaluations" :> ReqBody '[JSON] EvaluationRequest :> Post '[JSON] EvaluationResult 
  :<|> "getResultByEvaluation" :> ReqBody '[JSON] GameList :> Post '[JSON] ResultByEvaluation
  :<|> "testQuery" :> Get '[JSON] [Int]

chessApi :: Proxy (ChessApi (Handler b Service))
chessApi = Proxy


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
            :<|> uploadDB
            :<|> addEvaluations
            :<|> getResultByEvaluation
            :<|> testQuery
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
getGames requestData = do
  user <- currentUserName
  let dbKey = intToKeyDB $ gameRequestDB requestData
  db :: Maybe Database <- runPersist $ PsP.get dbKey
  dbp :: Maybe (Entity DatabasePermission) <- runPersist $ PsP.getBy $ UniqueDatabasePermission dbKey (maybe "" id user)
  let dbPublic = fmap databaseIsPublic db == Just True
  let userLoggedIn = isJust user
  let userCanRead = (isJust dbp) && (fmap (databasePermissionRead . PsP.entityVal) dbp == Just True)
  results <- if (dbPublic || (userLoggedIn && userCanRead)) then do
      results <- fmap gameGrouper $ runPersist $ getGames' requestData
      return results
    else return []
  return results

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

-- A useful handler for testing
nothingHandler :: Handler b Service ()
nothingHandler = do
  return ()

createAppUser :: T.Text -> Handler b Service ()
createAppUser userLogin = do
  time <- liftIO C.getCurrentTime
  runPersist $ Ps.insert_ $ AppUser (T.unpack userLogin) Nothing time
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
