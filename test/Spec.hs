{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, TypeFamilies #-}
-- import Test.HUnit
import Test.Hspec
import qualified Test.Hspec.Snap as Test
import           Snap.Core
import Snap.Snaplet
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad (unless)
import qualified Data.ByteString.Char8 as B
import Snap.Snaplet.Auth
import Database.Persist as P
import Database.Persist.Sql
import Snap.Snaplet.Persistent
import Database.Esqueleto
import Data.Either
import Data.Text as T (Text, pack)
import Data.List as L
import Data.Aeson
import Data.Maybe as M
import Data.Aeson.Types
import Data.Time
import Data.Time.Clock.POSIX
import Data.Either.Combinators (rightToMaybe)

import Services.Helpers
import Test.Helpers as H
import Services.Service as S
import Services.DatabaseHelpers as DBH
import Services.Types
import Services.Openings
import qualified Test.Fixtures as Fix
import Debug.Trace (trace)

import Data.Attoparsec.Text (parseOnly)

import AppTypes
import qualified Application as App

main = hspec $ do
  -- testOpening
  testUserHandling
  testApi

dbName :: String
dbName = "test"

settings = getSettings Test

testApi :: Spec
testApi = Test.snap (route (App.routes True)) (App.app settings) $ beforeAll_ doIO $ do

  describe "In the database functions," $ do

    it "there was at least one database stored" $ do
      dbDatabases :: [Entity Database] <- liftIO $ H.inBackend (DBH.connString dbName) $ selectList [] []
      Test.shouldEqual (not (null dbDatabases)) True

--     it "the databases function returns the right results" $ do
--       -- Logging in as a new user that doesn't own any databases
--         username <- liftIO getTimeString
--         Test.eval $ loginForApi username
--         Test.eval S.nothingHandler
--         res <- Test.eval S.getDatabases
--         dbDatabases :: [Entity Database] <- liftIO $ H.inBackend (DBH.connString dbName) $ selectList [(P.==.) DatabaseIsPublic True] []
--         Test.shouldEqual (L.length res) (L.length dbDatabases)

--     it "the eval averages are returned for every player" $ do
--       (_, players, dbGames) <- playerGameData dbName
--       res <- Test.eval $ S.gameEvaluations $ S.WrappedGameList $ fmap dbKey dbGames
--       Test.shouldEqual (L.length res) (L.length players)

--     it "the move summaries are returned for every player" $ do
--       (_, players, dbGames) <- playerGameData dbName
--       res <- Test.eval $ S.gameEvaluations $ WrappedGameList $ fmap dbKey dbGames
--       Test.shouldEqual (L.length res) (L.length players)

--     it "one should only get games for the private databases that a user owns" $ do
--       let userName = "testUser" :: String
--       let backendInsert row = liftIO $ H.inBackend (DBH.connString dbName) $ P.insert row
--       Test.eval $ loginForApi userName
--       user <- fromMaybe "" <$> Test.eval S.currentUserName
--       dbTime <- liftIO getTimeInt
--       db :: Key Database <- backendInsert $ Database ("temp" ++ show dbTime) False
--       dbp <- backendInsert $ DatabasePermission db user True True True
--       p <- backendInsert $ Player db "temp" "temp"
--       t <- backendInsert $ Tournament db "temp"
--       game <- backendInsert $ Game db p p 1 t "" Nothing Nothing
--       ga <- backendInsert $ GameAttribute game "test" "test"
--       let request = S.getGames $ GameRequestData (dbKeyInt db) []
--       games :: [GameDataFormatted] <- Test.eval request
--       Test.shouldEqual (L.length games) 1 

--       Test.eval $ loginForApi "temp2"
--       gamesForNewUser <- Test.eval request
--       Test.shouldEqual (L.length gamesForNewUser) 0

--   describe "In the API," $ do

--     it "databases should return what's public in the table" $ do 
--       -- Logging in as a user that doesn't own any databases
--       username <- liftIO getTimeString
--       Test.eval $ loginForApi username
--       Test.eval S.nothingHandler
--       res <- Test.get "/databases"
--       let apiDatabases = parseDB res
--       dbDatabases :: [Entity Database] <- liftIO $ H.inBackend (DBH.connString dbName) $ selectList [(P.==.) DatabaseIsPublic True] []
--       let values = fromMaybe [] apiDatabases
--       Test.shouldEqual (L.length values) (L.length dbDatabases)

--     it "a bad URL should 404" $ Test.get "/badApi" >>= Test.should404

--     it "It should not return a user if no user is set" $ do
--       let userName = "testUser"
--       res <- Test.get "/user"
--       let isFound = L.isInfixOf userName $ show res
--       Test.shouldNotBeTrue isFound

--     it "It should return a user if it is set" $ do
--       let userName = "testUser" :: String
--       Test.eval $ loginForApi userName
--       res <- Test.get "/user"
--       let isFound = L.isInfixOf userName $ show res
--       Test.shouldBeTrue isFound

--     describe "If I query the function to get games then" $ do

--       it "it doesn't return anything for a non-existent database" $ do
--         dbDatabases :: [Entity Database] <- liftIO $ H.inBackend (DBH.connString dbName) $ selectList [] []
--         dbTournaments :: [Entity Tournament] <- liftIO $ H.inBackend (DBH.connString dbName) $ selectList [] []
--         let nonExistingDB = L.maximum (fmap dbKey dbTournaments) + 1
--         let tournamentIds = fmap dbKey dbTournaments
--         let requestData = GameRequestData nonExistingDB tournamentIds
--         res :: [GameDataFormatted] <- Test.eval $ S.getGames requestData
--         Test.shouldEqual (L.length res) 0

--       it "it does return values for a database that has games" $ do
--         dbDatabases :: [Entity Database] <- liftIO $ H.inBackend (DBH.connString dbName) $ selectList [] []
--         dbTournaments :: [Entity Tournament] <- liftIO $ H.inBackend (DBH.connString dbName) $ selectList [] []
--         dbGames :: [Entity Game] <- liftIO $ H.inBackend (DBH.connString dbName) $ selectList [] []
--         Test.shouldBeTrue (length dbGames > 0)
--         let firstGame = L.head dbGames 
--         let gamesForDB = L.filter (\v -> gameDatabaseId (entityVal v) == gameDatabaseId (entityVal firstGame)) dbGames
--         let firstGameInt = L.head $ catMaybes $ fmap keyInt $ P.keyToValues $ gameDatabaseId $ entityVal firstGame
--         let tournamentIds = fmap dbKey dbTournaments
--         let requestData = GameRequestData firstGameInt tournamentIds
--         res <- Test.eval $ S.getGames requestData
--         Test.shouldEqual (L.length res) (L.length gamesForDB)



-- registerTestUser :: Handler App.App App.App (Either AuthFailure AuthUser)
-- registerTestUser = with App.auth $ registerUser (B.pack "hi") (B.pack "you")

parseDB :: Test.TestResponse -> Maybe [Entity Database]
parseDB (Test.Json _ bs) = decode bs
parseDB _ = Nothing

-- Setting up the database fixtures. This function is time-intensive, and run
-- once before a set of tests is executed. These tests do not modify the data.
-- Thus we want to set up this function that re-running it doesn't delete and
-- re-insert the data, which shortens the time for running the tests
doIO :: IO ()
doIO = do
  let dbName = "test"
  dbDatabases :: [Entity Database] <- H.inBackend (DBH.connString dbName) $ selectList [] []

  let alreadyRun = False -- length dbDatabases > 0

  let runEval = True
  let onlyContinueEval = False
  let settings = Fix.FixtureSettings dbName runEval onlyContinueEval
  unless alreadyRun $ Fix.runJob settings
  return ()

defaultDBName = "dummy games"

getTimeInt :: IO Int
getTimeInt = getCurrentTime >>= pure . (1000*) . utcTimeToPOSIXSeconds >>= pure . round

getTimeString :: IO String
getTimeString = fmap show getTimeInt

-- getDefaultDBId = fmap (P.entityKey . L.head) $ liftIO $ H.inBackend (DBH.connString dbName) $ selectList [(P.==.) DatabaseName defaultDBName] []

-- expectedTest text expected parse = do
--   let result = parseOnly parse $ pack text
--   result `shouldBe` expected


-- playerGameData dbName = do
--   defaultDBId <- getDefaultDBId
--   players :: [Entity Player] <- liftIO $ H.inBackend (DBH.connString dbName) $ selectList [(P.==.) PlayerDatabaseId defaultDBId] []
--   dbGames :: [Entity Game] <- liftIO $ H.inBackend (DBH.connString dbName) $ selectList [(P.==.) GameDatabaseId defaultDBId] []
--   return (defaultDBId, players, dbGames)

-- testOpening :: Spec
-- testOpening = describe "The opening module" $ do

--   it "can correctly parse the code" $ 
--     expectedTest "A00" (Right "A00") openingCodeParser

--   it "can parse a simple opening name" $ 
--     expectedTest "Test' Opening" (Right "Test' Opening") openingNameParser

--   it "can parse a incorrect opening name" $ 
--     expectedTest "Test' Opening\n" (Right "Test' Opening") openingNameParser

--   it "can parse an opening name with comments" $ 
--     expectedTest "Test' Opening; comments" (Right "Test' Opening") openingNameParser

--   it "can parse a game move" $ 
--     expectedTest "1. e4 1/2" (Right "1. e4") openMoveParser

--   it "can parse the game list" $ 
--     expectedTest "A00 A\n1.a3 1/2" (Right (ListData "A00" "A" "1.a3")) parseListData

--   it "can parse the game list with newlines" $ 
--     expectedTest "A00 A\n1.a3 1/2\n\n\n" (Right (ListData "A00" "A" "1.a3")) parseListData

-- userName = "testUser2" :: B.ByteString
-- params = Test.params [("login", userName), ("password", "password")]

loginForApi userName = do 
  withTop App.service $ do
    S.createFullUser userName "password"
    S.forceLoginFromEmail userName

testUserHandling :: Spec
testUserHandling = Test.snap (route (App.routes True)) (App.app settings) $ do
    describe "In the testing functions," $ do
      it "running createFullUser adds an AppUser" $ do
        userName :: String <- liftIO getTimeString
        dbUsersBefore :: [Entity AppUser] <- liftIO $ H.inBackend (DBH.connString dbName) $ selectList [] []
        Test.eval $ withTop App.service $ S.createFullUser userName "password"
        dbUsersAfter :: [Entity AppUser] <- liftIO $ H.inBackend (DBH.connString dbName) $ selectList [] []
        Test.shouldEqual ((length dbUsersBefore) + 1) (length dbUsersAfter)

      it "forcing a login works" $ do
        userName :: String <- liftIO getTimeString

        runUser <- Test.eval $ do
          loginForApi userName
          withTop App.service S.currentUserName

        Test.shouldEqual (Just userName) runUser

-- settings = getSettings Test

-- tests :: Spec
-- tests = Test.snap (route (App.routes True)) (App.app settings) $ describe "Application" $ it "Register should 200" $ Test.post "/register" params >>= Test.should200

-- runWithLogin :: String -> String -> Handler b b () -> Handler b b ()
-- runWithLogin name password handler = do
--   x <- handler
--   return ()
