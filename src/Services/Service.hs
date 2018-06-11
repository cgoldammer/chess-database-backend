{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE PolyKinds         #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module Services.Service where

import GHC.Generics (Generic)
import Control.Lens (makeLenses, _1)
import qualified Control.Lens as Lens ((^.))
import Control.Monad.State.Class (get, gets)
import Data.Aeson (FromJSON, ToJSON)
import Snap.Snaplet (Snaplet, MonadSnaplet, SnapletInit, Handler, with, getSnapletUserConfig, makeSnaplet, nestSnaplet, addRoutes)
import Snap.Snaplet.PostgresqlSimple (Postgres, HasPostgres, getPostgresState, setLocalPostgresState, pgsInit)
import qualified Data.ByteString.Char8 as B (pack, ByteString)
import Data.List (groupBy)
import qualified Data.Text as T (Text, pack, unpack)
import Database.Persist (insert, insert_)
import qualified Database.Persist.Postgresql as PsP (get, getBy, entityVal)
import Database.Esqueleto hiding (get)
import Database.Persist.Sql (rawSql)

import Control.Monad.IO.Class (liftIO, MonadIO)
import Snap.Snaplet.Persistent (PersistState, HasPersistPool, getPersistPool, initPersistGeneric, runPersist)
import Data.Maybe (listToMaybe, isJust, fromMaybe)
import Servant.API hiding (GET)
import Servant (serveSnap, Server)
import Data.Proxy (Proxy(..))
import Data.Time.Clock (getCurrentTime)
import Data.Map (toList)
import Data.IORef (IORef, readIORef, writeIORef, newIORef)

import Control.Monad.Logger (runNoLoggingT, NoLoggingT)
import qualified Database.Persist.Postgresql as PG
import qualified Data.Configurator as DC (lookup)

import Services.Types
import qualified Services.Helpers as Helpers
import Services.DatabaseHelpers (connString, readTextIntoDB)
import qualified Test.Fixtures as TF
import qualified Test.Helpers as TH
import Services.Sql

import Data.Aeson
import Data.Text.Encoding
import qualified Data.ByteString.Lazy as LBS

data Service = Service {
    _servicePG :: Snaplet Postgres
  , _serviceDB :: Snaplet PersistState
  , _serviceCurrentUser :: IORef (Maybe String)}
makeLenses ''Service

type (Encoded a) = QueryParam "data" (JSONEncoded a)

-- |The module boils down to this api.
type ChessApi m = 
       "user"     :> Get '[JSON] (Maybe AppUser) 
  :<|> "players" :> Encoded DefaultSearchData :> Get '[JSON] [Entity Player]
  :<|> "tournaments" :> Encoded DefaultSearchData :> Get '[JSON] [Entity Tournament]
  :<|> "databases" :> Get '[JSON] [Entity Database]
  :<|> "evalResults" :> Encoded MoveRequestData :> Get '[JSON] [Helpers.EvalResult]
  :<|> "moveSummary" :> Encoded MoveRequestData :> Get '[JSON] [Helpers.MoveSummary]
  :<|> "dataSummary" :> Encoded DefaultSearchData :> Get '[JSON] DataSummary
  :<|> "resultPercentages" :> Encoded DefaultSearchData :> Get '[JSON] [ResultPercentage]
  :<|> "games" :> Encoded GameRequestData :> Get '[JSON] [GameDataFormatted]
  :<|> "gameEvaluations" :> Encoded WrappedGameList :> Get '[JSON] PlayerGameEvaluations
  :<|> "moveEvaluations" :> Encoded MoveEvaluationRequest :> Get '[JSON] [MoveEvaluationData]
  :<|> "test" :> QueryParam "testData" (JSONEncoded TestData) :> Get '[JSON] [Int]
  :<|> "uploadDB" :> ReqBody '[JSON] UploadData :> Post '[JSON] UploadResult
  :<|> "addEvaluations" :> ReqBody '[JSON] EvaluationRequest :> Post '[JSON] EvaluationResult 

chessApi :: Proxy (ChessApi (Handler b Service))
chessApi = Proxy

apiServer :: Server (ChessApi (Handler b Service)) (Handler b Service)
apiServer =
       getMyUser 
  :<|> maybeHandler getPlayers
  :<|> maybeHandler getTournaments 
  :<|> getDatabases 
  :<|> maybeHandler getEvalResults 
  :<|> maybeHandler getMoveSummary 
  :<|> maybeHandler getDataSummary
  :<|> maybeHandler getResultPercentages
  :<|> maybeHandler getGames
  :<|> maybeHandler gameEvaluations
  :<|> maybeHandler moveEvaluationHandler
  :<|> maybeHandler testCall'
  :<|> uploadDB
  :<|> addEvaluations

-- We wrap the game list as a newtype so it can be passed nicely as JSON.
-- The code would work without wrapping, but, due to HTML intriciacies, lists don't
-- produce nice JSON, so the resulting URL would be extremely long.
data WrappedGameList = WrappedGameList { gameList :: GameList } deriving (Generic, FromJSON, ToJSON)

-- A newtype for JSON data sent as query parameter in get
-- requests
newtype JSONEncoded a = JSONEncoded { unJSONEncoded :: a }
  deriving (Eq, Show)

-- A way to decode parameters that are sent through get requests
instance (FromJSON a) => FromHttpApiData (JSONEncoded a) where
  parseQueryParam x = case eitherDecode $ LBS.fromStrict $ encodeUtf8 x of
    Left err -> Left (T.pack err)
    Right value -> Right (JSONEncoded value)

instance (ToJSON a) => ToHttpApiData (JSONEncoded a) where
  toQueryParam (JSONEncoded x) = decodeUtf8 $ LBS.toStrict $ encode x

-- Parsing the query parameters into JSON returns a `Maybe (JSONEncoded a)`. In practice,
-- I'll usually have a function `h :: a -> Handler b Service d`, so this function
-- creates the required handler from h and returning the monoid `mempty` if
-- the query could not get parsed
maybeHandler :: HasDefault d => (a -> Handler b Service d) -> Maybe (JSONEncoded a) -> Handler b Service d
maybeHandler h getData = do
  maybe (return defaultVal) (\enc -> h (unJSONEncoded enc)) getData

data TestData = TestData { testInt :: Int, testNames :: [String] } deriving (Show, Generic, FromJSON, ToJSON)

testCall' :: TestData -> Handler b Service [Int]
testCall' td = do
  return $ testInt td : fmap length (testNames td)

serviceInit :: String -> SnapletInit b Service
serviceInit dbName = makeSnaplet "chess" "Chess Service" Nothing $ do
  pg <- nestSnaplet "pg" servicePG pgsInit
  d <- nestSnaplet "db" serviceDB $ initPersistWithDB dbName (runMigrationUnsafe migrateAll)
  addRoutes chessRoutes
  usr <- liftIO $ newIORef Nothing
  return $ Service pg d usr

chessRoutes :: [(B.ByteString, Handler b Service ())]
chessRoutes = [("", serveSnap chessApi apiServer)]

type LoginUser = Maybe Int

mkSnapletPgPoolWithDB :: (MonadIO (m b v), MonadSnaplet m) => String -> m b v ConnectionPool
mkSnapletPgPoolWithDB dbName = do
    conf <- getSnapletUserConfig
    maybeSize <- liftIO $ DC.lookup conf "postgre-pool-size"
    let conStr = B.pack $ "host='localhost' dbname='chess_" ++ dbName ++ "' user='postgres'"
    let size = fromMaybe 1 maybeSize
    liftIO . runNoLoggingT $ PG.createPostgresqlPool conStr size

initPersistWithDB :: String -> SqlPersistT (NoLoggingT IO) a -> SnapletInit b PersistState
initPersistWithDB dbName = initPersistGeneric $ mkSnapletPgPoolWithDB dbName

currentUserName :: Handler b Service (Maybe String)
currentUserName = gets _serviceCurrentUser >>= (liftIO . readIORef)

changeUser :: Maybe String -> Handler b Service ()
changeUser value = gets _serviceCurrentUser >>= liftIO . flip writeIORef value

instance HasPersistPool (Handler b Service) where
  getPersistPool = with serviceDB getPersistPool

instance HasPostgres (Handler b Service) where
  getPostgresState = with servicePG get
  setLocalPostgresState = undefined

data DataSummary = DataSummary { 
    numberTournaments :: Int
  , numberGames :: Int
  , numberGameEvals :: Int
  , numberMoveEvals :: Int
} deriving (Generic, Show, Eq, ToJSON, FromJSON)

class HasDefault a where
  defaultVal :: a 

instance HasDefault ([a]) where
  defaultVal = []

instance HasDefault DataSummary where
  defaultVal = DataSummary 0 0 0 0

data DefaultSearchData = DefaultSearchData { searchDB :: Int } deriving (Generic, FromJSON, ToJSON, Show)
type QueryType = (Single Int, Single Int, Single Int, Single Int)

getDataSummary :: DefaultSearchData -> Handler b Service DataSummary
getDataSummary searchData = do
  let db = searchDB searchData
  let arguments = replicate 4 $ PersistInt64 (fromIntegral db)
  results :: [QueryType] <- runPersist $ rawSql dataSummaryQuery arguments
  let (Single numTournaments, Single numGames, Single numGameEvals, Single numMoveEvals) = head results
  return $ DataSummary numTournaments numGames numGameEvals numMoveEvals

data ResultPercentage = ResultPercentage {
    ownElo :: Int
  , opponentElo :: Int
  , winPercentage :: Int
  , drawPercentage :: Int} deriving (Generic, FromJSON, ToJSON, Show)

type ResultPercentageQueryResult = (Single Int, Single Int, Single Int, Single Int)

toResultPercentage :: ResultPercentageQueryResult -> ResultPercentage
toResultPercentage (Single ownRating, Single oppRating, Single winP, Single drawP) = ResultPercentage ownRating oppRating winP drawP

getPlayers :: DefaultSearchData -> Handler b Service [Entity Player]
getPlayers searchData = runPersist $ do
  let db = val $ intToKeyDB $ searchDB searchData
  select $ distinct $
    from $ \(p, g) -> do
      where_ $ ((g^.GamePlayerWhiteId ==. p^.PlayerId) ||. (g^.GamePlayerBlackId ==. p^.PlayerId)) &&. (g^.GameDatabaseId ==. db)
      return p

getDatabases :: Handler b Service [Entity Database]
getDatabases = do
  currentUser :: Maybe String <- currentUserName
  runPersist $ do
    dbsPublic <- select $ from $ \db -> do
      where_ (db^.DatabaseIsPublic)
      return db
    let searchUser = fromMaybe "" currentUser
    let searchCondition dbp = (dbp^.DatabasePermissionUserId ==. val searchUser) &&. (dbp^.DatabasePermissionRead ==. val True)
    let mergeCondition db dbp = dbp^.DatabasePermissionDatabaseId ==. db^.DatabaseId
    dbsPersonal <- select $ distinct $ 
      from $ \(db, dbp) -> do
        where_ $ mergeCondition db dbp &&. searchCondition dbp
        return db
    return $ dbsPublic ++ dbsPersonal
  
getTournaments :: DefaultSearchData -> Handler b Service [Entity Tournament]
getTournaments searchData = runPersist $ do
  let db = val $ intToKeyDB $ searchDB searchData
  select $ distinct $ 
    from $ \(t, g) -> do
      where_ $ (g^.GameDatabaseId ==. db) &&. (t^.TournamentId ==. g^.GameTournament)
      return t

intToKey :: Int -> Key Tournament
intToKey = toSqlKey . fromIntegral

intToKeyDB :: Int -> Key Database
intToKeyDB = toSqlKey . fromIntegral

intToKeyGame :: Int -> Key Game
intToKeyGame = toSqlKey . fromIntegral

evalData :: MoveRequestData -> Handler b Service ([Entity Player], [Helpers.EvalResult])
evalData mrData = do
  let db = moveRequestDB mrData
  let tournaments = moveRequestTournaments mrData
  let tournamentKeys = fmap intToKey tournaments
  players :: [Entity Player] <- getPlayers $ DefaultSearchData db
  evals <- getMoveEvals (intToKeyDB db) tournamentKeys
  return (players, evals)

getEvalResults :: MoveRequestData -> Handler b Service [Helpers.EvalResult]
getEvalResults = fmap snd . evalData

getMoveSummary :: MoveRequestData -> Handler b Service [Helpers.MoveSummary]
getMoveSummary mrData = do
  (playerKeys, evals) <- evalData mrData
  return $ Helpers.summarizeEvals playerKeys evals

selectEvalResults :: MonadIO m => Key Database -> [Key Tournament] -> SqlPersistT m [Helpers.EvalResult]
selectEvalResults db tournaments = do
  let tournamentMatch t = t ^. TournamentId `in_` valList tournaments
  let tournamentCondition t = if not (null tournaments) then tournamentMatch t else val True
  select $ 
    from $ \(me, g, t) -> do
    where_ $ (me ^. MoveEvalGameId ==. g ^. GameId) &&. (g ^. GameTournament ==. t ^. TournamentId) &&. tournamentCondition t &&. (g^.GameDatabaseId ==. val db)
    return (me, g)

getMoveEvals :: Key Database -> [Key Tournament] -> Handler b Service [Helpers.EvalResult]
getMoveEvals db tournaments = runPersist $ selectEvalResults db tournaments

printName :: GameAttributeId -> String
printName = show

gameRead :: Int -> String
gameRead = show

data MoveRequestData = MoveRequestData {
  moveRequestDB :: Int
, moveRequestTournaments :: [Int]
} deriving (Generic, FromJSON, ToJSON, Show)

type GameEvaluation = Int
type GameOutcome = Int
type PlayerKey = Int
type PlayerGameEvaluations = [(PlayerKey, [(GameEvaluation, GameOutcome)])]

parseEvalResults :: (Single Int, Single Int, Single Int, Single Int) -> (PlayerKey, GameEvaluation, GameOutcome)
parseEvalResults (_, Single playerId, Single evaluation, Single result) = (playerId, evaluation, result)

gameEvaluations :: WrappedGameList -> Handler b Service PlayerGameEvaluations
gameEvaluations wrapped = do
  let gl = gameList wrapped
  runPersist $ rawExecute viewQuery []
  results <- runPersist $ rawSql (substituteGameList evalQueryTemplate gl) []
  let parsed = fmap parseEvalResults results
  let grouped = fmap (\(_, b, c) -> (b, c)) <$> Helpers.groupWithVal (Lens.^._1) parsed
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
  evaluations <- liftIO $ sequenceA $ fmap (TF.doAndStoreEvaluationIO dbName) games
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
  let evaluatedIds = fmap entityKey evaluatedGames :: [Key Game]
  let difference = [g | g <- allGames, entityKey g `notElem` evaluatedIds]
  return $ if overwrite then allGames else difference


getDBName :: Handler b Service String
getDBName = do
  conf <- getSnapletUserConfig
  dbNameMaybe :: Maybe String <- liftIO $ DC.lookup conf "dbName"
  return $ fromMaybe "dev" dbNameMaybe

uploadDB :: UploadData -> Handler b Service UploadResult
uploadDB upload = do
  let (name, text) = (uploadName upload, uploadText upload)
  dbName <- getDBName
  (db, results) <- liftIO $ readTextIntoDB dbName name text False
  currentUser :: Maybe String <- currentUserName
  addDBPermission db currentUser
  return $ UploadResult $ Just $ length results

addDBPermission :: Key Database -> Maybe String -> Handler b Service (Key DatabasePermission)
addDBPermission dbResult userName = runPersist $ insert $ DatabasePermission dbResult (fromMaybe "" userName) True True False

data GameRequestData = GameRequestData {
    gameRequestDB :: Int
  , gameRequestTournaments :: [Int]
} deriving (Generic, FromJSON, ToJSON)

data MoveEvaluationRequest = MoveEvaluationRequest {
  moveEvalGames :: GameList
} deriving (Show, Generic, FromJSON)

getMoveEvaluationData :: MoveEvaluationRequest -> TH.DataAction [MoveEvaluationData]
getMoveEvaluationData (MoveEvaluationRequest gl) = do
  let gameIds = fmap intToKeyGame gl
  results :: [(Entity Game, Entity MoveEval)] <- select $  
    from $ \(g, me) -> do
      where_ $ (me ^. MoveEvalGameId ==. g ^. GameId) &&. ((g ^. GameId) `in_` valList gameIds)
      return (g, me)
  let filters = filter notAlreadyWinning . filter notAlreadyLosing . filter (highMoveLoss . moveEvalsMoveLoss)
  let cleaned = filters $ getEvalData results
  return cleaned

moveLossCutoff :: Int
moveLossCutoff = 200

highMoveLoss :: MoveLoss -> Bool
highMoveLoss (MoveLossMate _)= True
highMoveLoss (MoveLossCP x) = x >= moveLossCutoff

evalCutoff :: Int
evalCutoff = 300

notAlreadyWinning :: MoveEvaluationData -> Bool
notAlreadyWinning dat = evalWithColor <= evalCutoff
  where evalAfter = moveEvalsMoveEvalNext dat
        wasWhite = not $ moveEvalIsWhite evalAfter
        evalNum = fromMaybe 0 $ moveEvalEval evalAfter
        evalWithColor = if wasWhite then evalNum else (- evalNum)

notAlreadyLosing :: MoveEvaluationData -> Bool
notAlreadyLosing dat = evalWithColor <= evalCutoff
  where evalBest = moveEvalsMoveEval dat
        wasWhite = moveEvalIsWhite evalBest
        evalNum = fromMaybe 0 $ moveEvalEval evalBest
        evalWithColor = if wasWhite then (- evalNum) else evalNum

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
getEvalData dat = concat $ [evalGame game (withLag evals) | (game, evals) <- grouped]
  where grouped = toList $ (fmap . fmap) snd $ Helpers.groupWithVal fst dat :: [(Entity Game, [Entity MoveEval])]

evalGame :: Entity Game -> [(Entity MoveEval, Entity MoveEval)] -> [MoveEvaluationData]
evalGame g moveEvals = fmap (evalHelper g) moveEvals

evalHelper :: Entity Game -> (Entity MoveEval, Entity MoveEval) -> MoveEvaluationData
evalHelper ga (meE, meLaggedE) = MoveEvaluationData ga me meLagged (getMoveLoss me meLagged)
  where me = entityVal meE
        meLagged = entityVal meLaggedE

withLag :: [a] -> [(a, a)]
withLag [] = []
withLag (_ : []) = []
withLag (x1:x2:rest) = (x1, x2) : withLag (x2 : rest)

getMoveLoss :: MoveEval -> MoveEval -> MoveLoss
getMoveLoss meBefore meAfter = getMoveLossHelper evalBefore evalAfter mateBefore mateAfter
  where evalBefore = moveEvalEval meBefore
        evalAfter = moveEvalEval meAfter
        mateBefore = moveEvalMate meBefore
        mateAfter = moveEvalMate meAfter

getMoveLossHelper :: Maybe Int -> Maybe Int -> Maybe Int -> Maybe Int -> MoveLoss
getMoveLossHelper (Just before) (Just after) _ _ = MoveLossCP $ max (after - before) 0
getMoveLossHelper _ _ (Just _) (Just _) = MoveLossCP 0
getMoveLossHelper _ (Just _) (Just before) _ = MoveLossMate before
getMoveLossHelper _ _ _ _= MoveLossCP 0

getMyUser :: Handler b Service (Maybe AppUser)
getMyUser = currentUserName >>= runPersist . selectUser . fmap T.pack 

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
  dbp :: Maybe (Entity DatabasePermission) <- runPersist $ PsP.getBy $ UniqueDatabasePermission dbKey (fromMaybe "" usr)
  let dbPublic = fmap databaseIsPublic db == Just True
  let userLoggedIn = isJust usr
  let userCanRead = isJust dbp && (fmap (databasePermissionRead . PsP.entityVal) dbp == Just True)
  if dbPublic || (userLoggedIn && userCanRead)
    then fmap gameGrouper $ runPersist $ getGames' requestData
    else return []

groupSplitter :: [GameData] -> GameDataFormatted
groupSplitter ((g, t, pWhite, pBlack, ga) : rest) = GameDataFormatted g t pWhite pBlack allAttributes
  where allAttributes = ga : fmap (\(_, _, _, _, gat) -> gat) rest

gameDataEqual :: GameData -> GameData -> Bool
gameDataEqual gd gd' = gameKey gd == gameKey gd'
  where gameKey dat = entityKey (dat Lens.^._1)

gameGrouper :: [GameData] -> [GameDataFormatted]
gameGrouper allGames = groupSplitter <$> Data.List.groupBy gameDataEqual allGames

getGames' :: MonadIO m => GameRequestData -> SqlPersistT m [GameData]
getGames' requestData = do
  let db = intToKeyDB $ gameRequestDB requestData
  let tournaments = gameRequestTournaments requestData
  let tournamentKeys = fmap intToKey tournaments
  let tournamentMatch t = t ^. TournamentId `in_` valList tournamentKeys
  select $ 
    from $ \(g, t, pWhite, pBlack, ga) -> do
      where_ $ 
            (g ^. GameTournament ==. t ^. TournamentId) 
        &&. (g ^. GameDatabaseId ==. val db)
        &&. (if not (null tournaments) then tournamentMatch t else not_ (tournamentMatch t))
        &&. (pWhite ^. PlayerId ==. g ^. GamePlayerWhiteId) 
        &&. (pBlack ^. PlayerId ==. g ^. GamePlayerBlackId) 
        &&. (ga ^. GameAttributeGameId ==. g ^. GameId)
      return (g, t, pWhite, pBlack, ga)

getResultPercentages :: DefaultSearchData -> Handler b Service [ResultPercentage]
getResultPercentages searchData = do
  let db = searchDB searchData
  results :: [ResultPercentageQueryResult] <- runPersist $ rawSql resultPercentageQuery [PersistInt64 (fromIntegral db)]
  return $ fmap toResultPercentage results

-- |A useful handler for testing
nothingHandler :: Handler b Service ()
nothingHandler = return ()

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
