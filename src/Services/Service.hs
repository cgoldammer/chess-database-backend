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

import System.Environment (lookupEnv)
import GHC.Generics (Generic)
import Control.Lens (makeLenses, _1)
import qualified Control.Lens as Lens ((^.))
import Control.Monad.State.Class (get, gets)
import Data.Aeson (FromJSON, ToJSON)
import Snap.Core (modifyResponse, setResponseStatus)
import Snap.Snaplet (Snaplet, MonadSnaplet, SnapletInit, Handler, with, getSnapletUserConfig, makeSnaplet, nestSnaplet, addRoutes)
import Snap.Snaplet.PostgresqlSimple (Postgres, HasPostgres, getPostgresState, setLocalPostgresState, pgsInit)
import qualified Data.ByteString.Char8 as B (pack, ByteString)
import Data.List (groupBy, intercalate)
import qualified Data.Text as T (Text, pack, unpack, length)
import qualified Data.Text.Lazy as LT (pack)
import Database.Persist (insert, insert_)
import qualified Database.Persist.Postgresql as PsP (get, getBy, entityVal)
import Database.Esqueleto hiding (get)
import Database.Persist.Sql (rawSql)

import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad (liftM2)
import Snap.Snaplet.Persistent (PersistState, HasPersistPool, getPersistPool, initPersistGeneric, runPersist)
import Data.Maybe (listToMaybe, isJust, fromMaybe)
import Servant.API hiding (GET)
import Servant (serveSnap, Server)
import Data.Proxy (Proxy(..))
import Data.Time.Clock (getCurrentTime)
import Data.Map (toList)
import Data.IORef (IORef, readIORef, writeIORef, newIORef)
import Control.Concurrent (MVar, newMVar, takeMVar, putMVar, forkIO, threadDelay)

import Network.HTTP.Client.TLS (getGlobalManager)
import Network.HTTP.Client (Manager)
import Network.Mail.Mime.SES (SES(..), usEast1, renderSendMailSES)
import Network.Mail.Mime (Address(..), simpleMail')

import Control.Monad.Logger (runNoLoggingT, NoLoggingT)
import qualified Database.Persist.Postgresql as PG
import qualified Data.Configurator as DC (lookup)

import Services.Types
import Services.Tasks
import Services.Helpers (groupWithVal, EvalResult, MoveSummary, summarizeEvals, dbKeyInt, intToKeyGame, intToKeyDB, intToKey)
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
  , _serviceCurrentUser :: IORef (Maybe String)
  , _serviceAllTasks :: MVar AllTasks}
makeLenses ''Service

type (Encoded a) = QueryParam "data" (JSONEncoded a)

-- |The module boils down to this api.
type ChessApi m = 
       "user"     :> Get '[JSON] (Maybe AppUser) 
  :<|> "players" :> Encoded DefaultSearchData :> Get '[JSON] [Entity Player]
  :<|> "tournaments" :> Encoded DefaultSearchData :> Get '[JSON] [Entity Tournament]
  :<|> "databases" :> Get '[JSON] [Entity Database]
  :<|> "evalResults" :> Encoded MoveRequestData :> Get '[JSON] [EvalResult]
  :<|> "moveSummary" :> Encoded MoveRequestData :> Get '[JSON] [MoveSummary]
  :<|> "dataSummary" :> Encoded DefaultSearchData :> Get '[JSON] DataSummary
  :<|> "resultPercentages" :> Encoded DefaultSearchData :> Get '[JSON] [ResultPercentage]
  :<|> "games" :> Encoded GameRequestData :> Get '[JSON] [GameDataFormatted]
  :<|> "gameEvaluations" :> Encoded WrappedGameList :> Get '[JSON] PlayerGameEvaluations
  :<|> "moveEvaluations" :> Encoded MoveEvaluationRequest :> Get '[JSON] [MoveEvaluationData]
  :<|> "test" :> QueryParam "testData" (JSONEncoded TestData) :> Get '[JSON] [Int]
  :<|> "uploadDB" :> ReqBody '[JSON] UploadData :> Post '[JSON] UploadResult
  :<|> "addEvaluations" :> ReqBody '[JSON] EvaluationRequest :> Post '[JSON] ()
  :<|> "games2" :> Get '[JSON] [(Entity Game, Entity Tournament, Maybe (Entity OpeningVariation))]
  :<|> "sendFeedback" :> ReqBody '[JSON] FeedbackData :> Post '[JSON] ()

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
  :<|> uploadDBHelper
  :<|> addEvaluations
  :<|> getGames2
  :<|> sendFeedback

data FeedbackData = FeedbackData { fbText :: String, fbEmail :: String } deriving (Generic, FromJSON)

sendFeedback :: FeedbackData -> Handler b Service ()
sendFeedback (FeedbackData feedbackText feedbackEmail) = do
  accessCode <- liftIO $ lookupEnv "AWS_ACCESS"
  secretCode <- liftIO $ lookupEnv "AWS_SECRET"
  let codes = liftM2 (,) accessCode secretCode
  let fullText = intercalate ": " [feedbackEmail, feedbackText]
  liftIO $ maybe doNothing (uncurry (sendEmail fullText)) codes

fromEmail :: String
fromEmail = "cg@chrisgoldammer.com"

fromAddress :: Address
fromAddress = Address Nothing (T.pack fromEmail)

toAddress :: Address
toAddress = Address Nothing "goldammer.christian@gmail.com"

type EmailBody = String
type AwsAccess = String
type AwsSecret = String

sendEmail :: EmailBody -> AwsAccess -> AwsSecret -> IO ()
sendEmail body access secret = do
  let ses = SES (B.pack fromEmail) [] (B.pack access) (B.pack secret) Nothing usEast1
  let subject = "feedback"
  let mail = simpleMail' toAddress fromAddress subject (LT.pack body)
  manager :: Manager <- getGlobalManager
  renderSendMailSES manager ses mail
  return ()

    
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

  -- Creating an MVar with a list of evaluation tasks
  -- and spinning of a thread to run those evaluations.
  tasks <- liftIO $ newMVar emptyTasks
  liftIO $ forkIO $ runEvalThread dbName tasks

  return $ Service pg d usr tasks

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
    rpOwnElo :: Int
  , rpOpponentElo :: Int
  , rpEvaluation :: Int
  , rpWinPercentage :: Int
  , rpDrawPercentage :: Int
  , rpNumberEvals :: Int

} deriving (Generic, FromJSON, ToJSON, Show)

type ResultPercentageQueryResult = (Single Int, Single Int, Single Int, Single Int, Single Int, Single Int)

toResultPercentage :: ResultPercentageQueryResult -> ResultPercentage
toResultPercentage (Single ownRating, Single oppRating, Single evalGroup, Single winP, Single drawP, Single numberEvals) = ResultPercentage ownRating oppRating evalGroup winP drawP numberEvals

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

evalData :: MoveRequestData -> Handler b Service ([Entity Player], [EvalResult])
evalData mrData = do
  let db = moveRequestDB mrData
  let tournaments = moveRequestTournaments mrData
  let tournamentKeys = fmap intToKey tournaments
  players :: [Entity Player] <- getPlayers $ DefaultSearchData db
  evals <- getMoveEvals (intToKeyDB db) tournamentKeys
  return (players, evals)

getEvalResults :: MoveRequestData -> Handler b Service [EvalResult]
getEvalResults = fmap snd . evalData

getMoveSummary :: MoveRequestData -> Handler b Service [MoveSummary]
getMoveSummary mrData = do
  (playerKeys, evals) <- evalData mrData
  return $ summarizeEvals playerKeys evals

selectEvalResults :: MonadIO m => Key Database -> [Key Tournament] -> SqlPersistT m [EvalResult]
selectEvalResults db tournaments = do
  let tournamentMatch t = t ^. TournamentId `in_` valList tournaments
  let tournamentCondition t = if not (null tournaments) then tournamentMatch t else val True
  select $ 
    from $ \(me, g, t) -> do
    where_ $ (me ^. MoveEvalGameId ==. g ^. GameId) &&. (g ^. GameTournament ==. t ^. TournamentId) &&. tournamentCondition t &&. (g^.GameDatabaseId ==. val db)
    return (me, g)

getMoveEvals :: Key Database -> [Key Tournament] -> Handler b Service [EvalResult]
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
  let grouped = fmap (\(_, b, c) -> (b, c)) <$> groupWithVal (Lens.^._1) parsed
  return $ toList grouped

data UploadData = UploadData { uploadName :: String, uploadText :: T.Text } deriving (Generic, FromJSON)
data UploadResult = UploadSuccess Int | UploadFailure RequestError deriving (Generic, ToJSON)

data RequestError = UploadTooBig | NotLoggedIn deriving (Generic, ToJSON)

data EvaluationRequest = EvaluationRequest { evaluationDB :: Int, evaluationOverwrite :: Bool } deriving (Generic, FromJSON)
type EvaluationResult = Int

-- A helper function so we can wait in tenth of seconds.
waitTenths :: Int -> IO ()
waitTenths = threadDelay . (*100000)

addEvaluations :: EvaluationRequest -> Handler b Service ()
addEvaluations request = do
  let dbKey = intToKeyDB $ evaluationDB request
  let overwrite = evaluationOverwrite request
  dbName <- getDBName
  games :: [Entity Game] <- liftIO $ gamesInDB dbName dbKey overwrite
  liftIO $ print $ "Games: " ++ show (length games)
  user <- currentUserName
  let newTask = Task "Evaluation" games dbName user
  m <- gets _serviceAllTasks
  tasks <- liftIO $ takeMVar m
  let afterTasks = addTask tasks newTask
  liftIO $ putMVar m $ afterTasks
  -- store the evaluations to file so I know what's currently running
  liftIO $ writeFile "/home/cg/chess-backend/log/tasks.log" $ show afterTasks
  return ()


doNothing :: IO ()
doNothing = return ()

runTask :: Task -> IO ()
runTask (Task _ games dbName _) = do
  sequenceA (fmap (TF.doAndStoreEvaluationIO dbName) games) >> doNothing

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


handleNoUser :: Handler b Service UploadResult
handleNoUser = do
  modifyResponse $ setResponseStatus 403 "Data too big"
  return $ UploadFailure NotLoggedIn

uploadDBHelper :: UploadData -> Handler b Service UploadResult
uploadDBHelper upload = do
  currentUser <- currentUserName
  maybe handleNoUser (\_ -> uploadDB upload) currentUser

-- Uploading a database from the db. The pgn is included
-- in the `text` property of the JSON.
-- We are rejecting all uploads that exceed a certain size.
-- This is hacky, because optimally we'd want to server to not even respond
-- to those requests, but I haven't figured out if this is possible
-- to do in Nginx (I'd want a separate limit for this endpoint)
uploadDB :: UploadData -> Handler b Service UploadResult
uploadDB upload = do
  let (name, text) = (uploadName upload, uploadText upload)
  let textLength = T.length text
  let maxTextLength = 200 * 1024
  if textLength > maxTextLength
    then do
      modifyResponse $ setResponseStatus 403 "Data too big"
      return $ UploadFailure UploadTooBig
    else do
      dbName <- getDBName
      (db, results) <- liftIO $ readTextIntoDB dbName name text False
      currentUser :: Maybe String <- currentUserName
      addDBPermission db currentUser
      -- Storing evaluations for the database in an asynchronous thread.
      addEvaluations (EvaluationRequest (dbKeyInt db) False)
      return $ UploadSuccess $ length results

addDBPermission :: Key Database -> Maybe String -> Handler b Service (Key DatabasePermission)
addDBPermission dbResult user = runPersist $ insert $ DatabasePermission dbResult (fromMaybe "" user) True True False

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

moveEvaluationHandler :: MoveEvaluationRequest -> Handler b Service [MoveEvaluationData]
moveEvaluationHandler mer = runPersist $ getMoveEvaluationData mer

data MoveEvaluationData = MoveEvaluationData {
  moveEvalsGame :: Entity Game
, moveEvalsMoveEval :: MoveEval
, moveEvalsMoveLoss :: MoveLoss
} deriving (Show, Generic, ToJSON)

data MoveLoss = MoveLossCP Int | MoveLossMate Int deriving (Show, Generic, ToJSON)

getEvalData :: [(Entity Game, Entity MoveEval)] -> [MoveEvaluationData]
getEvalData dat = concat $ [evalGame game evals | (game, evals) <- grouped]
  where grouped = toList $ (fmap . fmap) snd $ groupWithVal fst dat :: [(Entity Game, [Entity MoveEval])]

evalGame :: Entity Game -> [Entity MoveEval] -> [MoveEvaluationData]
evalGame g moveEvals = fmap (evalHelper g) moveEvals

evalHelper :: Entity Game -> Entity MoveEval -> MoveEvaluationData
evalHelper ga meEntity = MoveEvaluationData ga me $ getMoveLoss me
  where me = entityVal meEntity

getMoveLoss :: MoveEval -> MoveLoss
getMoveLoss (MoveEval _ _ isWhite movePlayed moveBest evalAfter evalBefore mateAfter mateBefore _) = if bestMovePlayed then (MoveLossCP) 0 else moveLossBasic
  where moveLossBasic = getMoveLossHelper isWhite evalBefore evalAfter mateBefore mateAfter
        bestMovePlayed = movePlayed == Just moveBest

getMoveLossHelper :: Bool -> Maybe Int -> Maybe Int -> Maybe Int -> Maybe Int -> MoveLoss
getMoveLossHelper True (Just before) (Just after) _ _ = MoveLossCP $ (before - after)
getMoveLossHelper False (Just before) (Just after) _ _ = MoveLossCP $ (after - before)
getMoveLossHelper _ _ _ (Just _) (Just _) = MoveLossCP 0
getMoveLossHelper _ _ (Just _) (Just before) _ = MoveLossMate (-before)
getMoveLossHelper _ _ _ _ _= MoveLossCP 0

getMyUser :: Handler b Service (Maybe AppUser)
getMyUser = currentUserName >>= runPersist . selectUser . fmap T.pack 

type GameData = (Entity Game, Entity Tournament, Maybe (Entity OpeningVariation), Entity Player, Entity Player, Entity GameAttribute)

data GameDataFormatted = GameDataFormatted {
    gameDataGame :: Entity Game
  , gameDataTournament :: Entity Tournament
  , gameDataOpening :: Maybe (Entity OpeningVariation)
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
groupSplitter ((g, t, ov, pWhite, pBlack, ga) : rest) = GameDataFormatted g t ov pWhite pBlack allAttributes
  where allAttributes = ga : fmap (\(_, _, _, _, _, gat) -> gat) rest

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
    from $ \(g `InnerJoin` t `InnerJoin` pWhite `InnerJoin` pBlack `InnerJoin` ga `LeftOuterJoin` ov) -> do
      on (g ^. GameOpeningVariation ==. ov ?. OpeningVariationId)
      on (ga ^. GameAttributeGameId ==. g ^. GameId)
      on (pBlack ^. PlayerId ==. g ^. GamePlayerBlackId) 
      on (pWhite ^. PlayerId ==. g ^. GamePlayerWhiteId) 
      on (g ^. GameTournament ==. t ^. TournamentId) 
      where_ $ 
            (g ^. GameDatabaseId ==. val db)
        &&. (if not (null tournaments) then tournamentMatch t else not_ (tournamentMatch t))
      return (g, t, ov, pWhite, pBlack, ga)


getGames2 :: Handler b Service [(Entity Game, Entity Tournament, Maybe (Entity OpeningVariation))]
getGames2 = runPersist $ do
  select $ 
    from $ \(g `InnerJoin` t `LeftOuterJoin` ov) -> do
      on (g ^. GameOpeningVariation ==. ov ?. OpeningVariationId)
      on (g ^. GameTournament ==. t ^. TournamentId) 
      return (g, t, ov)

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
