{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Services.Service where

import Control.Lens (_1, makeLenses, over, _head, _tail, each)
import qualified Control.Lens as Lens ((^.))
import Control.Exception (Exception, SomeException, throw)
import Control.Monad.State.Class (get, gets)
import Data.Char (toLower)
import Data.Aeson (FromJSON, ToJSON, toJSON, eitherDecode, encode, genericToJSON, defaultOptions)
import Data.Aeson.Types (fieldLabelModifier)
import qualified Data.ByteString.Char8 as B (ByteString, pack)
import Data.List (groupBy, intercalate)
import qualified Data.Text as T (Text, length, pack, unpack)
import qualified Data.Text.Lazy as LT (pack)
import Database.Esqueleto hiding (get)
import Database.Persist (insert, insert_)
import qualified Database.Persist.Postgresql as PsP (entityVal, get, getBy)
import Database.Persist.Sql (rawSql)
import GHC.Generics (Generic)
import Snap.Core (modifyResponse, setResponseStatus)
import Snap.Snaplet
  ( Handler
  , MonadSnaplet
  , Snaplet
  , SnapletLens
  , SnapletInit
  , addRoutes
  , getSnapletUserConfig
  , makeSnaplet
  , nestSnaplet
  , with
  , withTop
  )
import Snap.Snaplet.PostgresqlSimple
  ( HasPostgres
  , Postgres
  , getPostgresState
  , pgsInit
  , setLocalPostgresState
  )
import System.Environment (lookupEnv)

import Snap.Snaplet.Auth
  ( userLogin
  , currentUser
  , createUser
  , forceLogin
  , logout
  , withBackend
  , AuthUser(..)
  , UserId(..)
  , IAuthBackend(..)
  , AuthManager(..))

import Control.Concurrent
  ( MVar
  , forkIO
  , newMVar
  , putMVar
  , takeMVar
  , threadDelay
  )
import Control.Monad (liftM2, when, unless)
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Map  as MP 
import Data.Maybe (fromMaybe, isJust, listToMaybe, catMaybes)
import Data.Proxy (Proxy(..))
import Data.Time.Clock (getCurrentTime)
import Servant (Server, serveSnap)
import Servant.API hiding (GET)
import Snap.Snaplet.Persistent
  ( HasPersistPool
  , PersistState
  , getPersistPool
  , initPersistGeneric
  , runPersist
  )

import Network.HTTP.Client (Manager)
import Network.HTTP.Client.TLS (getGlobalManager)
import Network.Mail.Mime (Address(..), simpleMail')
import Network.Mail.Mime.SES (SES(..), renderSendMailSES, usEast1)

import Control.Monad.Logger (NoLoggingT, runNoLoggingT)
import qualified Data.Configurator as DC (lookup)
import qualified Database.Persist.Postgresql as PG

import Services.DatabaseHelpers (connString, readTextIntoDB)
import Services.Helpers
  ( EvalResult
  , MoveSummary
  , dbKey
  , dbKeyInt
  , groupWithVal
  , intToKey
  , intToKeyDB
  , intToKeyGame
  , summarizeEvals
  )
import Services.Sql
import Services.Tasks
import Services.Types
import qualified Test.Fixtures as TF
import qualified Test.Helpers as TH

import qualified Data.ByteString.Lazy as LBS
import Data.Text.Encoding
data Service b = Service {
    _servicePG :: Snaplet Postgres
  , _serviceDB :: Snaplet PersistState
  , _serviceAllTasks :: MVar AllTasks
  , _serviceAuth :: SnapletLens b (AuthManager b)
}
makeLenses ''Service

type (Encoded a) = QueryParam "data" (JSONEncoded a)

-- |The module boils down to this api.
type ChessApi m = 
  "user" :> Get '[JSON] (Maybe AppUser) :<|>
  "players" :> Encoded DefaultSearchData :> Get '[JSON] [Entity Player] :<|> 
  "tournaments" :> Encoded DefaultSearchData :> Get '[JSON] [Entity Tournament] :<|>
  "databases" :> Get '[JSON] [Entity Database] :<|>
  "databaseStats" :> Get '[JSON] [DBResultFull] :<|>
  "evalResults" :> Encoded GameRequestData :> Get '[JSON] [EvalResult] :<|>
  "moveSummary" :> Encoded GameRequestData :> Get '[JSON] [MoveSummary] :<|>
  "dataSummary" :> Encoded DefaultSearchData :> Get '[JSON] DataSummary :<|>
  "resultPercentages" :> Encoded DefaultSearchData :> Get '[JSON] [ResultPercentage] :<|>
  "games" :> Encoded GameRequestData :> Get '[JSON] [GameDataFormatted] :<|>
  "gameEvaluations" :> Encoded GameRequestData :> Get '[JSON] PlayerGameEvaluations :<|>
  "moveEvaluations" :> Encoded GameRequestData :> Get '[JSON] [MoveEvaluationData] :<|>
  "moveEvaluationsFromIds" :> Encoded Ids :> Get '[JSON] [MoveEvaluationData] :<|>
  "uploadDB" :> ReqBody '[JSON] UploadData :> Post '[JSON] UploadResult :<|>
  "addEvaluations" :> ReqBody '[JSON] EvaluationRequest :> Post '[JSON] () :<|>
  "sendFeedback" :> ReqBody '[JSON] FeedbackData :> Post '[JSON] ()

chessApi :: Proxy (ChessApi (Handler b (Service b)))
chessApi = Proxy

-- apiServer :: Server (ChessApi (Handler b (Service b))) (Handler b (Service b))
apiServer =
  getMyUser :<|> 
  validatedHandler getPlayers :<|> 
  validatedHandler getTournaments :<|>
  getDatabases :<|>
  getDatabaseStats :<|>
  validatedHandler getEvalResults :<|>
  validatedHandler getMoveSummary :<|>
  validatedHandler getDataSummary :<|>
  validatedHandler getResultPercentages :<|>
  validatedHandler getGames :<|>
  validatedHandler gameEvaluations :<|>
  validatedHandler moveEvaluationHandler :<|>
  validatedHandler moveEvaluationFromIdHandler :<|>
  uploadDBHelper :<|>
  addEvaluations :<|>
  sendFeedback


data FeedbackData = FeedbackData { fbText :: String, fbEmail :: String } deriving (Generic, FromJSON)

trySendEmail :: String -> String -> String -> IO ()
trySendEmail subject to body = do
  let toAdd = Address Nothing (T.pack to)
  accessCode <- liftIO $ lookupEnv "AWS_ACCESS"
  secretCode <- liftIO $ lookupEnv "AWS_SECRET"
  let codes = liftM2 (,) accessCode secretCode
  liftIO $ maybe doNothing (uncurry (sendEmail subject toAdd body)) codes

sendFeedback :: FeedbackData -> Handler b (Service b) ()
sendFeedback (FeedbackData feedbackText feedbackEmail) =
  liftIO $
  trySendEmail "Feedback" "goldammer.christian@gmail.com" $
  intercalate ": " [feedbackEmail, feedbackText]

fromEmail :: String
fromEmail = "cg@chrisgoldammer.com"

fromAddress :: Address
fromAddress = Address Nothing (T.pack fromEmail)

type EmailBody = String
type AwsAccess = String
type AwsSecret = String

sendEmail :: String -> Address -> EmailBody -> AwsAccess -> AwsSecret -> IO ()
sendEmail subject to body access secret = do
  let ses = SES (B.pack fromEmail) [] (B.pack access) (B.pack secret) Nothing usEast1
  let mail = simpleMail' to fromAddress (T.pack subject) (LT.pack body)
  manager :: Manager <- getGlobalManager
  renderSendMailSES manager ses mail
  return ()

    
-- We wrap the game list as a newtype so it can be passed nicely as JSON.
-- The code would work without wrapping, but, due to HTML intriciacies, lists don't
-- produce nice JSON, so the resulting URL would be extremely long.
newtype WrappedGameList = WrappedGameList
  { gameList :: GameList
  } deriving (Generic, FromJSON, ToJSON)

-- A newtype for JSON data sent as query parameter in get
-- requests
newtype JSONEncoded a = JSONEncoded
  { unJSONEncoded :: a
  } deriving (Eq, Show)

-- A way to decode parameters that are sent through get requests
instance (FromJSON a) => FromHttpApiData (JSONEncoded a) where
  parseQueryParam x = case eitherDecode $ LBS.fromStrict $ encodeUtf8 x of
    Left err -> Left (T.pack err)
    Right value -> Right (JSONEncoded value)

instance (ToJSON a) => ToHttpApiData (JSONEncoded a) where
  toQueryParam (JSONEncoded x) = decodeUtf8 $ LBS.toStrict $ encode x
  
validatedHandler ::
     (HasDefault d, QueryForDB q)
  => (q -> Handler b (Service b) d)
  -> Maybe (JSONEncoded q)
  -> Handler b (Service b) d
validatedHandler = maybeHandlerJSON . validateRequestForDB

-- Parsing the query parameters into JSON returns a `Maybe (JSONEncoded a)`. In practice,
-- I'll usually have a function `h :: a -> Handler b Service d`, so this function
-- creates the required handler from h and returning the monoid `mempty` if
-- the query could not get parsed
maybeHandlerJSON ::
     HasDefault d
  => (a -> Handler b (Service b) d)
  -> Maybe (JSONEncoded a)
  -> Handler b (Service b) d
maybeHandlerJSON h = maybe (return defaultVal) (h . unJSONEncoded)

maybeHandler ::
     HasDefault d
  => (a -> Handler b (Service b) d)
  -> Maybe a
  -> Handler b (Service b) d
maybeHandler h = maybe (return defaultVal) h


data TestData = TestData
  { testInt :: Int
  , testNames :: [String]
  } deriving (Show, Generic, FromJSON, ToJSON)

testCall' :: TestData -> Handler b (Service b) [Int]
testCall' td = return $ testInt td : fmap length (testNames td)

serviceInit :: String -> SnapletLens b (AuthManager b) -> SnapletInit b (Service b) 
serviceInit dbName auth  = makeSnaplet "chess" "Chess Service" Nothing $ do
  pg <- nestSnaplet "pg" servicePG pgsInit
  d <- nestSnaplet "db" serviceDB $ initPersistWithDB dbName (runMigrationUnsafe migrateAll)
  addRoutes chessRoutes

  -- Creating an MVar with a list of evaluation tasks
  -- and spinning of a thread to run those evaluations.
  tasks <- liftIO $ newMVar emptyTasks
  liftIO $ forkIO $ runEvalThread dbName tasks
  return $ Service pg d tasks auth

chessRoutes :: [(B.ByteString, Handler b (Service b) ())]
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

currentUserName :: Handler b (Service b) (Maybe String)
currentUserName = do
  lens <- gets _serviceAuth
  user <- withTop lens currentUser
  let login = fmap (T.unpack . userLogin) user :: Maybe String
  return login

type UserName = String
newtype LoginException = LoginUserDoesNotExist UserName deriving Show
instance Exception LoginException

getAuthUserFromEmail :: String -> Handler b (Service b) (Maybe AuthUser)
getAuthUserFromEmail userId = do
  auth <- gets _serviceAuth
  userIdInt <- getUserId userId

  let lookup i = withTop auth $ withBackend $ \r -> liftIO $ lookupByUserId r $ UserId (T.pack (show i))
  maybeHandler lookup userIdInt

forceLoginFromEmail :: String -> Handler b (Service b) (Maybe AppUser)
forceLoginFromEmail userId = do
  auth <- gets _serviceAuth
  au <- getAuthUserFromEmail userId
  let forceLoginHandler a = withTop auth $ logout >> forceLogin a >> return Nothing
  maybeHandler forceLoginHandler au
  getMyUser
  
selectUserId :: T.Text
selectUserId = T.pack "SELECT id FROM snap_auth_user WHERE login = ?"

type UserIdType = (Single Int)

getUserId :: String -> Handler b (Service b) (Maybe Int)
getUserId userId = do
  let arguments = [PersistText (T.pack userId)]
  userIds :: [UserIdType] <- runPersist $ rawSql selectUserId arguments
  return $ unSingle <$> listToMaybe userIds

-- changeUser :: Maybe String -> Handler b (Service b) ()
-- changeUser _ = do
--   return ()

instance HasPersistPool (Handler b (Service b)) where
  getPersistPool = with serviceDB getPersistPool

instance HasPostgres (Handler b (Service b)) where
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

instance HasDefault (Maybe a) where
  defaultVal = Nothing

instance HasDefault [a] where
  defaultVal = []

instance HasDefault DataSummary where
  defaultVal = DataSummary 0 0 0 0

class QueryForDB a where
  getDB :: a -> Int

instance QueryForDB DefaultSearchData where
  getDB = searchDB

instance QueryForDB GameRequestData where
  getDB = gameRequestDB

instance QueryForDB Ids where
  getDB = idDB

type DBQueryType = (Single Int, Single Int, Single Int, Single Int)

data DBResult = DBResult { 
  dbResultId :: Int
, dbResultGames :: Int
, dbResultGamesEval :: Int
, dbResultEvals :: Int
} deriving (Generic)

data DBResultFull = DBResultFull {
  dbResultNumbers :: DBResult
, dbResultDB :: Database
} deriving (Generic)

instance ToJSON DBResult where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = renameField "dbResult"}

instance ToJSON DBResultFull where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = renameField "dbResult"}

toDBResults :: DBQueryType -> DBResult
toDBResults (Single id, Single numGames, Single numGamesEval, Single numEvals) =
  DBResult id numGames numGamesEval numEvals

combineMaybes :: Maybe a -> Maybe b -> (a -> b -> c) -> Maybe c
combineMaybes (Just a) (Just b) f = Just (f a b)
combineMaybes _ _ _ = Nothing

getDatabaseStats :: Handler b (Service b) [DBResultFull]
getDatabaseStats = do
  dbs <- getDatabases
  let dbKeys = fmap (dbKeyInt . entityKey) dbs
  let sub = substituteName "databases"
  let query = sub dbQuery dbKeys

  results :: [DBQueryType] <- runPersist $ rawSql query []
  let groupedResults :: MP.Map Int DBResult = MP.fromList $ fmap (\r -> (dbResultId r, r)) $ fmap toDBResults results
  let groupedDBs :: MP.Map Int Database = MP.fromList $ fmap (\d -> ((dbKeyInt . entityKey ) d, entityVal d)) dbs

  -- Todo: Use a merge strategy that is likely much more efficient
  let combined = fmap (\i -> (MP.lookup i groupedResults, MP.lookup i groupedDBs)) dbKeys

  let dbResultsFull = catMaybes $ fmap (\(a, b) -> combineMaybes a b DBResultFull) combined
  return dbResultsFull


newtype DefaultSearchData = DefaultSearchData { searchDB :: Int } deriving (Generic, FromJSON, ToJSON, Show)
type QueryType = (Single Int, Single Int, Single Int, Single Int)

getDataSummary :: DefaultSearchData -> Handler b (Service b) DataSummary
getDataSummary searchData = do
  let db = searchDB searchData
  let arguments = replicate 4 $ PersistInt64 (fromIntegral db)
  results :: [QueryType] <- runPersist $ rawSql dataSummaryQuery arguments
  let (Single numTournaments, Single numGames, Single numGameEvals, Single numMoveEvals) = head results
  return $ DataSummary numTournaments numGames numGameEvals numMoveEvals

data ResultPercentage = ResultPercentage {
    rpOwnElo :: Int
  , rpOpponentElo :: Int
  , rpEvaluation :: Int
  , rpWinPercentage :: Int
  , rpDrawPercentage :: Int
  , rpNumberEvals :: Int

} deriving (Generic, FromJSON, ToJSON, Show)

type ResultPercentageQueryResult = (Single Int, Single Int, Single Int, Single Int, Single Int, Single Int)

toResultPercentage :: ResultPercentageQueryResult -> ResultPercentage
toResultPercentage (Single ownRating, Single oppRating, Single evalGroup, Single winP, Single drawP, Single numberEvals) =
  ResultPercentage ownRating oppRating evalGroup winP drawP numberEvals

getPlayers :: DefaultSearchData -> Handler b (Service b) [Entity Player]
getPlayers searchData =
  runPersist $ do
    let db = val $ intToKeyDB $ searchDB searchData
    select $
      distinct $
      from $ \(p, g) -> do
        where_ $
          ((g ^. GamePlayerWhiteId ==. p ^. PlayerId) ||.
           (g ^. GamePlayerBlackId ==. p ^. PlayerId)) &&.
          (g ^. GameDatabaseId ==. db)
        return p

getDatabases :: Handler b (Service b) [Entity Database]
getDatabases = do
  currentUserEvaluated :: Maybe String <- currentUserName
  liftIO $ print $ "Getting for " ++ show currentUserEvaluated
  runPersist $ do
    dbsPublic <-
      select $
      from $ \db -> do
        where_ (db ^. DatabaseIsPublic)
        return db
    let searchUser = fromMaybe "" currentUserEvaluated
    let searchCondition dbp =
          (dbp ^. DatabasePermissionUserId ==. val searchUser) &&.
          (dbp ^. DatabasePermissionRead ==. val True)
    let mergeCondition db dbp = dbp ^. DatabasePermissionDatabaseId ==. db ^. DatabaseId
    dbsPersonal <-
      select $
      distinct $
      from $ \(db, dbp) -> do
        where_ $ mergeCondition db dbp &&. searchCondition dbp
        return db
    return $ dbsPublic ++ dbsPersonal
  
getTournaments :: DefaultSearchData -> Handler b (Service b) [Entity Tournament]
getTournaments searchData = runPersist $ do
  let db = val $ intToKeyDB $ searchDB searchData
  select $ distinct $ 
    from $ \(t, g) -> do
      where_ $ (g^.GameDatabaseId ==. db) &&. (t^.TournamentId ==. g^.GameTournament)
      return t


validateRequestForDB :: QueryForDB q => (q -> Handler b (Service b) c) -> q -> Handler b (Service b) c
validateRequestForDB handler q = do
  let db = getDB q
  dbs <- getDatabases
  let keys = fmap (dbKeyInt . entityKey) dbs
  let found = db `elem` keys
  liftIO $ print $ "FOUND: " ++ show keys ++ show found ++ show db
  if found then handler q else fail "Wrong permission"

evalData :: GameRequestData -> Handler b (Service b) ([Entity Player], [EvalResult])
evalData (GameRequestData db tournaments) = do
  let tournamentKeys = fmap intToKey tournaments
  players :: [Entity Player] <- getPlayers $ DefaultSearchData db
  evals <- getMoveEvals (intToKeyDB db) tournamentKeys
  return (players, evals)

getEvalResults :: GameRequestData -> Handler b (Service b) [EvalResult]
getEvalResults = fmap snd . evalData

getMoveSummary :: GameRequestData -> Handler b (Service b) [MoveSummary]
getMoveSummary grData = do
  (playerKeys, evals) <- evalData grData
  return $ summarizeEvals playerKeys evals

selectEvalResults :: MonadIO m => Key Database -> [Key Tournament] -> SqlPersistT m [EvalResult]
selectEvalResults db tournaments = do
  let tournamentMatch t = t ^. TournamentId `in_` valList tournaments
  let tournamentCondition t = if not (null tournaments) then tournamentMatch t else val True
  select $ 
    from $ \(me, g, t) -> do
    where_ $ (me ^. MoveEvalGameId ==. g ^. GameId) &&. (g ^. GameTournament ==. t ^. TournamentId) &&. tournamentCondition t &&. (g^.GameDatabaseId ==. val db)
    return (me, g)

getMoveEvals :: Key Database -> [Key Tournament] -> Handler b (Service b) [EvalResult]
getMoveEvals db tournaments = runPersist $ selectEvalResults db tournaments

printName :: GameAttributeId -> String
printName = show

gameRead :: Int -> String
gameRead = show

type GameEvaluation = Int
type GameOutcome = Int
type PlayerKey = Int
type PlayerGameEvaluations = [(PlayerKey, [(GameEvaluation, GameOutcome)])]

parseEvalResults ::
     (Single Int, Single Int, Single Int, Single Int)
  -> (PlayerKey, GameEvaluation, GameOutcome)
parseEvalResults (_, Single playerId, Single evaluation, Single result) =
  (playerId, evaluation, result)

gameEvaluations :: GameRequestData -> Handler b (Service b) PlayerGameEvaluations
gameEvaluations grd = do
  games <- getJustGames grd
  let gl = fmap dbKey games
  runPersist $ rawExecute viewQuery []
  results <- runPersist $ rawSql (substituteGameList evalQueryTemplate gl) []
  let parsed = fmap parseEvalResults results
  let grouped = fmap (\(_, b, c) -> (b, c)) <$> groupWithVal (Lens.^._1) parsed
  return $ MP.toList grouped

data UploadData = UploadData
  { uploadName :: String
  , uploadText :: T.Text
  } deriving (Generic, FromJSON)

data UploadResult
  = UploadSuccess Int
  | UploadFailure RequestError
  deriving (Generic, ToJSON)

data RequestError = UploadTooBig | NotLoggedIn deriving (Generic, ToJSON)

data EvaluationRequest = EvaluationRequest
  { evaluationDB :: Int
  , evaluationOverwrite :: Bool
  } deriving (Generic, FromJSON)


type EvaluationResult = Int

-- A helper function so we can wait in tenth of seconds.
waitTenths :: Int -> IO ()
waitTenths = threadDelay . (*100000)

addEvaluations :: EvaluationRequest -> Handler b (Service b) ()
addEvaluations request = do
  let keyForDB = intToKeyDB $ evaluationDB request
  canWrite <- canWriteToDB keyForDB
  unless canWrite $ fail "wrong permission"

  let overwrite = evaluationOverwrite request
  dbName <- getDBName
  games :: [Entity Game] <- liftIO $ gamesInDB dbName keyForDB overwrite
  liftIO $ print $ "Games: " ++ show (length games)
  user <- currentUserName
  let newTask = Task "Evaluation" games dbName user
  m <- gets _serviceAllTasks
  tasks <- liftIO $ takeMVar m
  let afterTasks = addTask tasks newTask
  liftIO $ putMVar m afterTasks
  -- store the evaluations to file so I know what's currently running
  liftIO $ writeFile "/home/cg/chess-backend/log/tasks.log" $ show afterTasks
  return ()


doNothing :: IO ()
doNothing = return ()

runTask :: Task -> IO ()
runTask (Task _ games dbName _) = traverse (TF.storeEvaluationIO dbName) games >> doNothing

-- The thread handler to run evaluations. The idea here is that
-- we want to be able to asynchronously add evaluation tasks as they come
-- in from user requests, but we only want to run at most one task
-- at a time so we don't overload the CPU. In other words, this is a FIFO
-- queue for tasks. 
-- This will likely be a bottleneck in the future, so expect this to change
-- as more users upload databases.
runEvalThread :: String -> MVar AllTasks -> IO ()
runEvalThread dbName m = do
  allTasks <- takeMVar m
  liftIO $ writeFile "/home/cg/chess-backend/log/tasks.log" $ show allTasks
  putMVar m allTasks
  let active = taskActive allTasks
  maybe doNothing (handleActiveTask m) active
  waitTenths 10
  runEvalThread dbName m
    
handleActiveTask :: MVar AllTasks -> Task -> IO ()
handleActiveTask m task = do
  runTask task
  allTasksAfter <- takeMVar m
  putMVar m $ completeActiveTask allTasksAfter
  return ()
  
gamesInDB :: String -> Key Database -> Bool -> IO [Entity Game]
gamesInDB dbName keyForDB overwrite = TH.inBackend (connString dbName) $ do
  let db = val keyForDB
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


getDBName :: Handler b (Service b) String
getDBName = do
  _ <- getSnapletUserConfig
  dbNameMaybe :: Maybe String <- liftIO $ lookupEnv "type"
  liftIO $ print $ "Lookup: " ++ show dbNameMaybe
  return $ fromMaybe "dev" dbNameMaybe

handleNoUser :: Handler b (Service b) UploadResult
handleNoUser = do
  modifyResponse $ setResponseStatus 403 "Data too big"
  return $ UploadFailure NotLoggedIn

uploadDBHelper :: UploadData -> Handler b (Service b) UploadResult
uploadDBHelper upload = do
  currentUserEvaluated <- currentUserName
  maybe handleNoUser (\_ -> uploadDB upload) currentUserEvaluated

-- Uploading a database from the db. The pgn is included
-- in the `text` property of the JSON.
-- We are rejecting all uploads that exceed a certain size.
-- This is hacky, because optimally we'd want to server to not even respond
-- to those requests, but I haven't figured out if this is possible
-- to do in Nginx (I'd want a separate limit for this endpoint)
uploadDB :: UploadData -> Handler b (Service b) UploadResult
uploadDB upload = do
  let (name, text) = (uploadName upload, uploadText upload)
  let textLength = T.length text
  let maxTextLength = 1000 * 1024
  if textLength > maxTextLength
    then do
      modifyResponse $ setResponseStatus 403 "Data too big"
      return $ UploadFailure UploadTooBig
    else do
      dbName <- getDBName
      currentUserEvaluated :: Maybe String <- currentUserName
      (db, results) <- liftIO $ readTextIntoDB dbName name text False currentUserEvaluated
      runPersist transactionSave
      addDBPermission db currentUserEvaluated
      -- Storing evaluations for the database in an asynchronous thread.
      addEvaluations (EvaluationRequest (dbKeyInt db) False)
      return $ UploadSuccess $ length results

canWriteToDB :: Key Database -> Handler b (Service b) Bool
canWriteToDB keyForDB = do
  usr <- currentUserName
  dbp :: Maybe (Entity DatabasePermission) <-
    runPersist $ PsP.getBy $ UniqueDatabasePermission keyForDB (fromMaybe "" usr)
  let canWrite = maybe False (databasePermissionWrite . entityVal) dbp
  return canWrite


addDBPermission :: Key Database -> Maybe String -> Handler b (Service b) (Key DatabasePermission)
addDBPermission dbResult user = runPersist $ insert $ DatabasePermission dbResult (fromMaybe "" user) True True False

data Ids = Ids { idDB :: Int, idValues :: [Int] } deriving (Generic, FromJSON)

data GameRequestData = GameRequestData {
    gameRequestDB :: Int
  , gameRequestTournaments :: [Int]
} deriving (Generic, FromJSON, ToJSON)

newtype MoveEvaluationRequest = MoveEvaluationRequest {
  moveEvalGames :: GameList
} deriving (Show, Generic, FromJSON)


getMoveEvaluationData :: Bool -> Key Database -> [Key Game] -> TH.DataAction [MoveEvaluationData]
getMoveEvaluationData doFilter db gameIds = do
  results :: [(Entity Game, Entity MoveEval)] <- select $  
    from $ \(g, me) -> do
      where_ $ (me ^. MoveEvalGameId ==. g ^. GameId) &&. ((g ^. GameId) `in_` valList gameIds) &&.
               (g ^. GameDatabaseId ==. val db)
      return (g, me)
  let filters = filter notAlreadyWinning . filter notAlreadyLosing . filter (highMoveLoss . moveEvalsMoveLoss)
  let activeFilter = if doFilter then filters else id
  let cleaned = activeFilter $ getEvalData results
  return cleaned

moveLossCutoff :: Int
moveLossCutoff = 200

highMoveLoss :: MoveLoss -> Bool
highMoveLoss (MoveLossMate _) = True
highMoveLoss (MoveLossCP x) = x >= moveLossCutoff

evalCutoff :: Int
evalCutoff = 300

notAlreadyWinning :: MoveEvaluationData -> Bool
notAlreadyWinning dat = evalWithColor <= evalCutoff
  where eval = moveEvalsMoveEval dat
        wasWhite = moveEvalIsWhite eval
        evalNum = fromMaybe 0 $ moveEvalEval eval
        evalWithColor = if wasWhite then evalNum else (- evalNum)

notAlreadyLosing :: MoveEvaluationData -> Bool
notAlreadyLosing dat = evalWithColor >= (-evalCutoff)
  where eval = moveEvalsMoveEval dat
        wasWhite = moveEvalIsWhite eval
        evalNum = fromMaybe 0 $ moveEvalEvalBest eval
        evalWithColor = if wasWhite then evalNum else (- evalNum)

moveEvaluationFromIdHandler :: Ids -> Handler b (Service b) [MoveEvaluationData]
moveEvaluationFromIdHandler (Ids db values) = do
  let keyDB = intToKeyDB db
  runPersist $ getMoveEvaluationData False keyDB (fmap intToKeyGame values)

moveEvaluationHandler :: GameRequestData -> Handler b (Service b) [MoveEvaluationData]
moveEvaluationHandler grd = do
  games <- getJustGames grd
  let keyDB = intToKeyDB $ gameRequestDB grd
  runPersist $ getMoveEvaluationData True keyDB (fmap entityKey games)

data MoveEvaluationData = MoveEvaluationData {
  moveEvalsGame :: Entity Game
, moveEvalsMoveEval :: MoveEval
, moveEvalsMoveLoss :: MoveLoss
} deriving (Show, Generic)

instance ToJSON MoveEvaluationData where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = renameField "moveEvals"}

data MoveLoss = MoveLossCP Int | MoveLossMate Int deriving (Show, Generic, ToJSON)

getEvalData :: [(Entity Game, Entity MoveEval)] -> [MoveEvaluationData]
getEvalData dat = concat [evalGame game evals | (game, evals) <- grouped]
  where grouped = MP.toList $ fmap snd <$> groupWithVal fst dat :: [(Entity Game, [Entity MoveEval])]

evalGame :: Entity Game -> [Entity MoveEval] -> [MoveEvaluationData]
evalGame = fmap . evalHelper

evalHelper :: Entity Game -> Entity MoveEval -> MoveEvaluationData
evalHelper ga meEntity = MoveEvaluationData ga me $ getMoveLoss me
  where me = entityVal meEntity

getMoveLoss :: MoveEval -> MoveLoss
getMoveLoss (MoveEval _ _ isWhite movePlayed moveBest evalAfter evalBefore mateAfter mateBefore _ _ _ _) =
  if bestMovePlayed
    then MoveLossCP 0
    else moveLossBasic
  where
    moveLossBasic = getMoveLossHelper isWhite evalBefore evalAfter mateBefore mateAfter
    bestMovePlayed = movePlayed == Just moveBest

getMoveLossHelper :: Bool -> Maybe Int -> Maybe Int -> Maybe Int -> Maybe Int -> MoveLoss
getMoveLossHelper True (Just before) (Just after) _ _ = MoveLossCP $ before - after
getMoveLossHelper False (Just before) (Just after) _ _ = MoveLossCP $ after - before
getMoveLossHelper _ _ _ (Just _) (Just _) = MoveLossCP 0
getMoveLossHelper _ _ (Just _) (Just before) _ = MoveLossMate (-before)
getMoveLossHelper _ _ _ _ _= MoveLossCP 0

getMyUser :: Handler b (Service b) (Maybe AppUser)
getMyUser = currentUserName >>= runPersist . selectUser . fmap T.pack 

type GameData = (Entity Game, Entity Tournament, Maybe (Entity OpeningVariation), Maybe (Entity OpeningLine), Entity Player, Entity Player, Entity GameAttribute)

data GameDataFormatted = GameDataFormatted {
    gameDataGame :: Entity Game
  , gameDataTournament :: Entity Tournament
  , gameDataOpening :: Maybe (Entity OpeningVariation)
  , gameDataOpeningLine :: Maybe (Entity OpeningLine)
  , gameDataPlayerWhite :: Entity Player
  , gameDataPlayerBlack :: Entity Player
  , gameDataAttributes :: [Entity GameAttribute]} deriving (Generic)

instance Eq GameDataFormatted
  where g == g' = gameDataGame g == gameDataGame g'

instance Show GameDataFormatted
  where show = fmap show gameDataGame 

renameField :: String -> String -> String
renameField toDrop s = lowerFirst $ drop (length toDrop) s

lowerFirst :: String -> String
lowerFirst = over _head toLower . over (_tail.each) id

instance ToJSON GameDataFormatted where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = renameField "gameData"}

getGamesHandler ::
     GameRequestData -> (GameRequestData -> SqlPersistM [a]) -> Handler b (Service b) [a]
getGamesHandler requestData getter = do
  usr <- currentUserName
  let keyForDB = intToKeyDB $ gameRequestDB requestData
  db :: Maybe Database <- runPersist $ PsP.get keyForDB
  dbp :: Maybe (Entity DatabasePermission) <-
    runPersist $ PsP.getBy $ UniqueDatabasePermission keyForDB (fromMaybe "" usr)
  let dbPublic = fmap databaseIsPublic db == Just True
  let userLoggedIn = isJust usr
  let userCanRead =
        isJust dbp && (fmap (databasePermissionRead . PsP.entityVal) dbp == Just True)
  if dbPublic || (userLoggedIn && userCanRead)
    then runPersist $ getter requestData
    else return []

getGames :: GameRequestData -> Handler b (Service b) [GameDataFormatted]
getGames requestData = gameGrouper <$> getGamesHandler requestData getGames'

getJustGames :: GameRequestData -> Handler b (Service b) [Entity Game]
getJustGames requestData = getGamesHandler requestData getJustGames'

groupSplitter :: [GameData] -> GameDataFormatted
groupSplitter ((g, t, ov, ol, pWhite, pBlack, ga):rest) =
  GameDataFormatted g t ov ol pWhite pBlack allAttributes
  where
    allAttributes = ga : restAttributes
    restAttributes = fmap (\(_, _, _, _, _, _, r) -> r) rest :: [Entity GameAttribute]

gameDataEqual :: GameData -> GameData -> Bool
gameDataEqual gd gd' = gameKey gd == gameKey gd'
  where gameKey dat = entityKey (dat Lens.^._1)

gameGrouper :: [GameData] -> [GameDataFormatted]
gameGrouper allGames = groupSplitter <$> Data.List.groupBy gameDataEqual allGames

getJustGames' :: MonadIO m => GameRequestData -> SqlPersistT m [Entity Game]
getJustGames' gr = fmap (Lens.^. _1) <$> getGames' gr
  
getGames' :: MonadIO m => GameRequestData -> SqlPersistT m [GameData]
getGames' (GameRequestData dbInt tournaments) = do
  let db = intToKeyDB dbInt
  let tournamentKeys = fmap intToKey tournaments
  let tournamentMatch t = t ^. TournamentId `in_` valList tournamentKeys
  select $ 
    from $ \(g `InnerJoin` t `InnerJoin` pWhite `InnerJoin` pBlack `InnerJoin` ga `LeftOuterJoin` ov `LeftOuterJoin` ol) -> do
      on (ov ?. OpeningVariationLine ==. ol ?. OpeningLineId)
      on (g ^. GameOpeningVariation ==. ov ?. OpeningVariationId)
      on (ga ^. GameAttributeGameId ==. g ^. GameId)
      on (pBlack ^. PlayerId ==. g ^. GamePlayerBlackId) 
      on (pWhite ^. PlayerId ==. g ^. GamePlayerWhiteId) 
      on (g ^. GameTournament ==. t ^. TournamentId) 
      where_ $ 
            (g ^. GameDatabaseId ==. val db)
        &&. (if not (null tournaments) then tournamentMatch t else not_ (tournamentMatch t))
      return (g, t, ov, ol, pWhite, pBlack, ga)


getResultPercentages :: DefaultSearchData -> Handler b (Service b) [ResultPercentage]
getResultPercentages searchData = do
  let db = searchDB searchData
  results <- runPersist $ rawSql resultPercentageQuery [PersistInt64 (fromIntegral db)]
  return $ fmap toResultPercentage results

-- |A useful handler for testing
nothingHandler :: Handler b (Service b) ()
nothingHandler = return ()

createFullUser :: String -> String -> Handler b (Service b) ()
createFullUser userEmail password = do
  lens <- gets _serviceAuth
  res <- withTop lens $ createUser (T.pack userEmail) (B.pack password)
  case res of
    Right authUser -> do
      let usId = userLogin authUser
      createAppUser usId
      withTop lens (forceLogin authUser)
    -- Left af -> return ()
  return ()
  

-- |Create a default app user. The id for the app user is the user name.
createAppUser :: T.Text -> Handler b (Service b) ()
createAppUser userLoginCreate = do
  time <- liftIO getCurrentTime
  runPersist $ insert_ $ AppUser (T.unpack userLoginCreate) Nothing time
  return ()

-- |Obtain the app user by user name.
selectUser :: MonadIO m => Maybe T.Text -> SqlPersistT m (Maybe AppUser)
selectUser (Just userId) = do
  users <- select $ from $ \usr -> do
    where_ $ usr ^. AppUserUserId ==. val (T.unpack userId)
    return usr
  return $ entityVal <$> listToMaybe users
selectUser Nothing = return Nothing
