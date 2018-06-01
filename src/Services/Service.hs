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
import Control.Lens (makeLenses)
import Control.Monad.State.Class (get, gets)
import qualified Control.Monad.State.Lazy as St (State, get)
import Data.Aeson (FromJSON, ToJSON)
import Snap.Core (writeBS)
import Snap.Snaplet (Snaplet, MonadSnaplet, SnapletInit, Handler, with, getSnapletUserConfig, makeSnaplet, nestSnaplet, addRoutes)
import Snap.Snaplet.PostgresqlSimple (Postgres, HasPostgres, getPostgresState, setLocalPostgresState, pgsInit)
import qualified Data.ByteString.Char8 as B (pack, ByteString)
import Data.List (groupBy)
import qualified Data.Text as T (Text, pack, unpack)
import qualified Data.Text.Lazy as TL (toStrict)
import Data.Text.Template (Context, substitute)
import Database.Persist (insert, insert_)
import qualified Database.Persist.Postgresql as PsP (get, getBy, entityVal)
import Database.Esqueleto hiding (get)
import Database.Persist.Sql (rawSql)

import Control.Monad.IO.Class (liftIO, MonadIO)
import Snap.Snaplet.Persistent (PersistState, HasPersistPool, getPersistPool, initPersistGeneric, runPersist)
import Data.Maybe (isJust)
import Servant.API hiding (GET)
import Servant (serveSnap, Server)
import Data.Maybe (listToMaybe)
import Data.Proxy (Proxy(..))
import Data.Time.Clock (getCurrentTime)
import Data.Map (Map, toList, mapWithKey)
import Data.List (lookup)
import Text.RawString.QQ (r)
import Data.IORef (IORef, readIORef, writeIORef, newIORef)

import Control.Monad.Logger (runNoLoggingT, NoLoggingT)
import qualified Database.Persist.Postgresql as PG
import qualified Data.Configurator as DC (lookup)

import Services.Types
import qualified Services.Helpers as Helpers
import Services.DatabaseHelpers (connString, readTextIntoDB, listToInClause)
import qualified Test.Fixtures as TF
import qualified Test.Helpers as TH

type UserState = St.State (Maybe String) (Maybe String)

user :: St.State (Maybe String) (Maybe String)
user = do n <- St.get
          return n

data Service = Service {
    _servicePG :: Snaplet Postgres
  , _serviceDB :: Snaplet PersistState
  , _serviceCurrentUser :: IORef (Maybe String)}
makeLenses ''Service

instance HasPersistPool (Handler b Service) where
  getPersistPool = with serviceDB getPersistPool

instance HasPostgres (Handler b Service) where
  getPostgresState = with servicePG get
  setLocalPostgresState = undefined

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
currentUserName = gets _serviceCurrentUser >>= (liftIO . readIORef)

changeUser :: Maybe String -> Handler b Service ()
changeUser value = gets _serviceCurrentUser >>= liftIO . ((flip writeIORef) value)

data ImpMove = ImpMove { 
  impMove :: String
, impBest :: String
, impWhite :: String
, impBlack :: String
, impMoveEval :: String
, impBestEval :: String
} deriving (Generic, FromJSON, ToJSON)


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
  liftIO $ print $ "Current user" ++ show currentUser
  dbs <- runPersist $ do
    dbsPublic <- select $ from $ \db -> do
      where_ (db^.DatabaseIsPublic)
      return db
    let searchUser = maybe "NOUSER" id currentUser
    let searchCondition dbp = (dbp^.DatabasePermissionUserId ==. (val searchUser)) &&. (dbp^.DatabasePermissionRead ==. (val True))
    let mergeCondition db dbp = dbp^.DatabasePermissionDatabaseId ==. db^.DatabaseId
    dbsPersonal <- select $ distinct $ 
      from $ \(db, dbp) -> do
        where_ $ (mergeCondition db dbp) &&. (searchCondition dbp)
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

intToKeyGame :: Int -> Key Game
intToKeyGame = toSqlKey . fromIntegral


data DataSummary = DataSummary { 
    numberTournaments :: Int
  , numberGames :: Int
  , numberGameEvals :: Int
  , numberMoveEvals :: Int
} deriving (Generic, Show, Eq, ToJSON, FromJSON)

dataSummaryQuery :: T.Text
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
  let (Single numTournaments, Single numGames, Single numGameEvals, Single numMoveEvals) = head results
  return $ DataSummary numTournaments numGames numGameEvals numMoveEvals

evalData :: MoveRequestData -> Handler b Service ([Entity Player], [Helpers.EvalResult])
evalData mrData = do
  let db = moveRequestDB mrData
  let tournaments = moveRequestTournaments mrData
  let tournamentKeys = fmap intToKey tournaments
  players :: [Entity Player] <- getPlayers $ DefaultSearchData db
  evals <- getMoveEvals (intToKeyDB db) tournamentKeys
  return (players, evals)

getEvalResults :: MoveRequestData -> Handler b Service [Helpers.EvalResult]
getEvalResults mrData = do
  (_, evals) <- evalData mrData
  return evals

getMoveSummary :: MoveRequestData -> Handler b Service [Helpers.MoveSummary]
getMoveSummary mrData = do
  (playerKeys, evals) <- evalData mrData
  return $ Helpers.summarizeEvals playerKeys evals

selectEvalResults :: MonadIO m => Key Database -> [Key Tournament] -> SqlPersistT m [Helpers.EvalResult]
selectEvalResults db tournaments = do
  let tournamentMatch t = t ^. TournamentId `in_` valList tournaments
  let tournamentCondition t = if (length tournaments > 0) then (tournamentMatch t) else (val True)
  results <- select $ 
    from $ \(me, g, t) -> do
    where_ $ (me ^. MoveEvalGameId ==. g ^. GameId) &&. (g ^. GameTournament ==. t ^. TournamentId) &&. (tournamentCondition t) &&. (g^.GameDatabaseId ==. val db)
    return (me, g)
  return results


getMoveEvals :: Key Database -> [Key Tournament] -> Handler b Service [Helpers.EvalResult]
getMoveEvals db tournaments = runPersist (selectEvalResults db tournaments)

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

data MoveRequestData = MoveRequestData {moveRequestDB :: Int, moveRequestTournaments :: [Int] } deriving (Generic, FromJSON, ToJSON, Show)

data ResultPercentage = ResultPercentage {
    ownElo :: Int
  , opponentElo :: Int
  , winPercentage :: Int
  , drawPercentage :: Int} deriving (Generic, FromJSON, ToJSON, Show)

data DefaultSearchData = DefaultSearchData { searchDB :: Int } deriving (Generic, FromJSON, ToJSON, Show)


type GameList = [Int]
type GameEvaluation = Int
type GameOutcome = Int
type PlayerKey = Int
type PlayerGameEvaluations = [(PlayerKey, [(GameEvaluation, GameOutcome)])]


parseEvalResults :: (Single Int, Single Int, Single Int, Single Int) -> (PlayerKey, GameEvaluation, GameOutcome)
parseEvalResults (_, Single playerId, Single evaluation, Single result) = (playerId, evaluation, result)


firstInt :: (Single Int, Single Int) -> Int
firstInt (Single a, _) = a


-- The evaluation of a move is eval - lag(eval, 1) by game. Top-code at 0.
-- then average by game and color.
-- After that, merge onto the game table and obtain the performance by each player.
viewQuery :: T.Text
viewQuery = [r|
CREATE OR REPLACE VIEW moveevals as (
  SELECT 
    game_id
  , is_white
  , move_number
  , greatest(is_white_int*(eval - next_eval), 0) as cploss
  FROM (
    SELECT 
      game_id
    , is_white
    , (is_white :: Int)*2-1 as is_white_int
    , move_number
    , eval
    , lead(eval) over (partition by game_id order by id) as next_eval
    , mate
    , fen
    , lead(mate) over (partition by game_id order by id) as next_mate
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
  SELECT game_id, player_white_id as player_id, avg(cploss)::Int as cploss, avg((game_result+1)*100/2)::Int as result from me_player WHERE is_white group by game_id, player_white_id
  UNION ALL
  SELECT game_id, player_black_id as player_id, avg(cploss)::Int as cploss, avg((-game_result+1)*100/2)::Int as result from me_player WHERE not is_white group by game_id, player_black_id;
|]


substituteGameList :: T.Text -> GameList -> T.Text
substituteGameList template ints = TL.toStrict $ substitute template cont
  where cont = context [("gameList", listToInClause ints)]

-- | Create 'Context' from association list.
context :: [(String, String)] -> Context
context assocs x = T.pack $ maybe err id . lookup (T.unpack x) $ assocs
  where err = error $ "Could not find key: " ++ T.unpack x

gameEvaluations :: GameList -> Handler b Service PlayerGameEvaluations
gameEvaluations gl = do
  runPersist $ rawExecute viewQuery []
  results <- runPersist $ rawSql (substituteGameList evalQueryTemplate gl) []
  let parsed = fmap parseEvalResults results
  let grouped = (fmap . fmap) (\(_, b, c) -> (b, c)) $ Helpers.groupWithVal (\(a, _, _) -> a) parsed
  return $ toList grouped



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
gamesInDB dbName dbKey overwrite = TH.inBackend (connString dbName) $ do
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
  (db, results) <- liftIO $ readTextIntoDB dbName name text False
  currentUser :: Maybe String <- currentUserName
  addDBPermission db currentUser
  return $ UploadResult $ Just $ length results

addDBPermission :: Key Database -> Maybe String -> Handler b Service (Key DatabasePermission)
addDBPermission dbResult userName = do
  permission <- runPersist $ insert $ DatabasePermission dbResult (maybe "" id userName) True True False
  return permission

data GameRequestData = GameRequestData {
    gameRequestDB :: Int
  , gameRequestTournaments :: [Int]
} deriving (Generic, FromJSON, ToJSON)

type ChessApi m = 
       "user"     :> Get '[JSON] (Maybe AppUser) 
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
  :<|> "gameEvaluations" :> ReqBody '[JSON] GameList :> Post '[JSON] PlayerGameEvaluations
  :<|> "moveEvaluations" :> ReqBody '[JSON] MoveEvaluationRequest :> Post '[JSON] [MoveEvaluationData]

data MoveEvaluationRequest = MoveEvaluationRequest {
  moveEvalGames :: GameList
} deriving (Show, Generic, FromJSON)

mer :: MoveEvaluationRequest
mer = MoveEvaluationRequest [1183]

da = getMoveEvaluationData mer
da' = getMoveEvaluationData $ MoveEvaluationRequest [1183]

back = TH.inBackend (connString "dev")

getMoveEvaluationData :: MoveEvaluationRequest -> TH.DataAction [MoveEvaluationData]
getMoveEvaluationData (MoveEvaluationRequest gl) = do
  let gameIds = fmap intToKeyGame gl
  results :: [(Entity Game, Entity MoveEval)] <- select $  
    from $ \(g, me) -> do
      where_ $ (me ^. MoveEvalGameId ==. g ^. GameId) &&. ((g ^. GameId) `in_` (valList gameIds))
      return (g, me)
  let cleaned = filter (highMoveLoss . moveEvalsMoveLoss) $ getEvalData results
  return cleaned

moveLossCutoff = 150
highMoveLoss :: MoveLoss -> Bool
highMoveLoss (MoveLossMate _)= True
highMoveLoss (MoveLossCP x) = x >= moveLossCutoff


moveEvaluationHandler :: MoveEvaluationRequest -> Handler b Service [MoveEvaluationData]
moveEvaluationHandler mer = runPersist $ getMoveEvaluationData mer

data MoveEvaluationData = MoveEvaluationData {
  moveEvalsGame :: Entity Game
, moveEvalsMoveEval :: MoveEval
, moveEvalsMoveEvalNext :: MoveEval
, moveEvalsMoveLoss :: MoveLoss
} deriving (Show, Generic, ToJSON)


data MoveLoss = MoveLossCP Int | MoveLossMate Int deriving (Show, Generic, ToJSON)

getEvalData :: [(Entity Game, Entity MoveEval)] -> [MoveEvaluationData]
getEvalData dat = concat lagged
  where lagged = [fmap (evalHelper (fst (head list))) (withLag (fmap snd list)) | list <- grouped]
        grouped = (fmap snd $ toList $ Helpers.groupWithVal (entityKey .fst) dat) :: [[(Entity Game, Entity MoveEval)]]

evalHelper :: Entity Game -> (Entity MoveEval, Entity MoveEval) -> MoveEvaluationData
evalHelper ga (meE, meLaggedE) = MoveEvaluationData ga me meLagged (getMoveLoss me meLagged)
  where me = entityVal meE
        meLagged = entityVal meLaggedE

withLag :: [a] -> [(a, a)]
withLag [] = []
withLag [x] = []
withLag (x1:x2:rest) = (x1, x2) : (withLag (x2 : rest))



moveEvalsQueryTemplate :: T.Text
moveEvalsQueryTemplate = [r|
  SELECT 
    g.id as game_id
  , player_white_id , player_black_id
  , is_white
  , eval, next_eval
  , mate, next_mate
  , fen
  FROM moveevals me
  JOIN game g
  ON me.game_id = g.id
  WHERE g.id in $gameList
  AND (
       cploss >= ? 
    OR ((mate is not null) AND (next_mate is null))
  )
|]

getMoveLoss :: MoveEval -> MoveEval -> MoveLoss
getMoveLoss meBefore meAfter = getMoveLossHelper evalBefore evalAfter mateBefore mateAfter
  where evalBefore = moveEvalEval meBefore
        evalAfter = moveEvalEval meAfter
        mateBefore = moveEvalMate meBefore
        mateAfter = moveEvalMate meAfter

getMoveLossHelper :: Maybe Int -> Maybe Int -> Maybe Int -> Maybe Int -> MoveLoss
getMoveLossHelper (Just before) (Just after) _ _ = MoveLossCP $ (max (after - before) 0)
getMoveLossHelper _ _ (Just _) (Just _) = MoveLossCP 0
getMoveLossHelper _ (Just _) (Just before) _ = MoveLossMate before
getMoveLossHelper _ _ _ _= MoveLossCP 0


chessApi :: Proxy (ChessApi (Handler b Service))
chessApi = Proxy

getMyUser :: Handler b Service (Maybe AppUser)
getMyUser = currentUserName >>= runPersist . selectUser . fmap T.pack 

apiServer :: Server (ChessApi (Handler b Service)) (Handler b Service)
apiServer =      getMyUser 
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
            :<|> gameEvaluations
            :<|> moveEvaluationHandler

type ResultPercentageQueryResult = (Single Int, Single Int, Single Int, Single Int)

toResultPercentage :: ResultPercentageQueryResult -> ResultPercentage
toResultPercentage (Single ownRating, Single oppRating, Single winP, Single drawP) = ResultPercentage ownRating oppRating winP drawP

resultPercentageQuery :: T.Text
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
  usr <- currentUserName
  let dbKey = intToKeyDB $ gameRequestDB requestData
  db :: Maybe Database <- runPersist $ PsP.get dbKey
  dbp :: Maybe (Entity DatabasePermission) <- runPersist $ PsP.getBy $ UniqueDatabasePermission dbKey (maybe "" id usr)
  let dbPublic = fmap databaseIsPublic db == Just True
  let userLoggedIn = isJust usr
  let userCanRead = (isJust dbp) && (fmap (databasePermissionRead . PsP.entityVal) dbp == Just True)
  results <- if (dbPublic || (userLoggedIn && userCanRead)) then do
      results <- fmap gameGrouper $ runPersist $ getGames' requestData
      return results
    else return []
  return results

groupSplitter :: [GameData] -> GameDataFormatted
groupSplitter ((g, t, pWhite, pBlack, ga) : rest) = GameDataFormatted g t pWhite pBlack allAttributes
  where allAttributes = ga : (fmap (\(_, _, _, _, gat) -> gat) rest)

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
  pg <- nestSnaplet "pg" servicePG pgsInit
  d <- nestSnaplet "db" serviceDB $ initPersistWithDB dbName (runMigrationUnsafe migrateAll)
  addRoutes chessRoutes
  usr <- liftIO $ newIORef $ Nothing
  return $ Service pg d usr

chessRoutes :: [(B.ByteString, Handler b Service ())]
chessRoutes = [("user2", writeBS "user test")] ++ [("", serveSnap chessApi apiServer)]


-- A useful handler for testing
nothingHandler :: Handler b Service ()
nothingHandler = do
  return ()

-- |Create a default app user. The id for the app user is the user name.
createAppUser :: T.Text -> Handler b Service ()
createAppUser userLogin = do
  time <- liftIO getCurrentTime
  runPersist $ insert_ $ AppUser (T.unpack userLogin) Nothing time
  return ()

-- |Obtain the app user by user name.
selectUser :: MonadIO m => Maybe T.Text -> SqlPersistT m (Maybe AppUser)
selectUser (Just userId) = do
  users <- select $ from $ \usr -> do
    where_ $ usr ^. AppUserUserId ==. val (T.unpack userId)
    return usr
  return $ entityVal <$> listToMaybe users
selectUser Nothing = return Nothing

userFields :: [B.ByteString]
userFields = fmap B.pack ["name"]
