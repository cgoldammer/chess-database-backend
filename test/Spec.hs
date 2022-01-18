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
import Control.Monad (void, when)

import Services.Helpers
import Test.Helpers as H
import Services.Service as S
import Services.DatabaseHelpers as DBH
import Services.Types
import Services.Openings
import qualified Test.Fixtures as Fix
import Snap.Snaplet.Session (commitSession)

import Data.Attoparsec.Text (parseOnly)

import AppTypes
import qualified Application as App

main = hspec $ do
  testOpening
  testUserHandling
  testApi

dbName :: String
dbName = "test"

connStringTest = DBH.connString dbName
inBackendTest = H.inBackend connStringTest

settings = getSettings Test

toTop :: Handler App.App (S.Service App.App) r -> Handler App.App App.App r
toTop = withTop App.service

insertUserData userName = do
  let backendInsert row = liftIO $ inBackendTest $ P.insert row
  loginForApi userName
  user <- fromMaybe "" <$> toTop S.currentUserName

  db :: Key Database <- backendInsert $ Database ("temp" ++ userName) False (Just user)
  dbp <- backendInsert $ DatabasePermission db user True True True
  p <- backendInsert $ Player db "temp" "temp"
  t <- backendInsert $ Tournament db "temp"
  game <- backendInsert $ Game db p p 1 t "" Nothing Nothing
  ga <- backendInsert $ GameAttribute game "test" "test"

  return db

nothingHandlerTest = toTop S.nothingHandler

loginAsAnotherRandomUser = do
  userName <- liftIO getTimeString
  loginForApi userName


getDBResults handler modifierAfterwards getKeys = Test.eval $ do
  userName <- liftIO getTimeString
  db <- insertUserData userName
  modifierAfterwards
  res <- toTop handler
  let keyUser = dbKeyInt db
  let filtered = filter (==keyUser) $ fmap getKeys res
  return $ length filtered

testApi :: Spec
testApi = Test.snap (route (App.routes True)) (App.app settings) $ beforeAll_ doIO $ do

  describe "In the database functions," $ do

    it "the databases functions returns the personal DB if logged in" $ do
      overlap <- getDBResults S.getDatabases nothingHandlerTest (dbKeyInt . entityKey)
      Test.shouldEqual overlap 1

    it "the databases functions does not return the personal DB if logged out" $ do
      overlap <- getDBResults S.getDatabases App.resetUser (dbKeyInt . entityKey)
      Test.shouldEqual overlap 0

    it "the database stats functions returns the personal DB if logged in" $ do
      overlap <- getDBResults S.getDatabaseStats nothingHandlerTest (dbResultId . dbResultNumbers)
      Test.shouldEqual overlap 1

    it "the database stats functions does not return the personal DB if logged out" $ do
      overlap <- getDBResults S.getDatabaseStats App.resetUser (dbResultId . dbResultNumbers)
      Test.shouldEqual overlap 0

    it "the database stats functions does not return the personal DB if logged in as another user" $ do
      overlap <- getDBResults S.getDatabaseStats loginAsAnotherRandomUser (dbResultId . dbResultNumbers)
      Test.shouldEqual overlap 0

    it "the eval averages are returned for every player" $ do
      (dbId, players, dbGames) <- playerGameData dbName
      res <- Test.eval $ toTop $ S.gameEvaluations $ S.GameRequestData (dbKeyInt dbId) []
      Test.shouldEqual (L.length res) (L.length players)

    it "one should only get games for the private databases that a user owns" $ do
      userName :: String <- liftIO getTimeString

      (games, gamesForNewUser) <- Test.eval $ do
        db <- insertUserData userName
        let request = toTop $ S.getGames $ GameRequestData (dbKeyInt db) []
        games :: [GameDataFormatted] <- request
        loginForApi $ userName ++ "_new"
        gamesForNewUser <- request
        return (games, gamesForNewUser)

      Test.shouldNotEqual games gamesForNewUser
      Test.shouldEqual (L.length games) 1 
      Test.shouldEqual (L.length gamesForNewUser) 0

  describe "In the API," $ do

    it "databases should return what's public in the table" $ do 
      -- Logging in as a user that doesn't own any databases
      userName <- liftIO getTimeString
      apiDatabases <- Test.eval $ do
        loginForApi userName
        toTop S.getDatabases
      dbDatabases :: [Entity Database] <- liftIO $ inBackendTest $ selectList [(P.==.) DatabaseIsPublic True] []
      Test.shouldEqual (L.length apiDatabases) (L.length dbDatabases)

    describe "If I query the function to get games then" $ do

      it "it doesn't return anything for a non-existent database" $ do
        dbDatabases :: [Entity Database] <- liftIO $ inBackendTest $ selectList [] []
        dbTournaments :: [Entity Tournament] <- liftIO $ inBackendTest $ selectList [] []
        let nonExistingDB = L.maximum (fmap dbKey dbTournaments) + 1
        let tournamentIds = fmap dbKey dbTournaments
        let requestData = GameRequestData nonExistingDB tournamentIds
        res :: [GameDataFormatted] <- Test.eval $ toTop $ S.getGames requestData
        Test.shouldEqual (L.length res) 0

      it "it does return values for a database that has games" $ do
        dbDatabases :: [Entity Database] <- liftIO $ inBackendTest $ selectList [] []
        dbTournaments :: [Entity Tournament] <- liftIO $ inBackendTest $ selectList [] []
        dbGames :: [Entity Game] <- liftIO $ inBackendTest $ selectList [] []
        Test.shouldBeTrue (length dbGames > 0)
        let firstGame = L.head dbGames 
        let gamesForDB = L.filter (\v -> gameDatabaseId (entityVal v) == gameDatabaseId (entityVal firstGame)) dbGames
        let firstGameInt = L.head $ catMaybes $ fmap keyInt $ P.keyToValues $ gameDatabaseId $ entityVal firstGame
        let tournamentIds = fmap dbKey dbTournaments
        let requestData = GameRequestData firstGameInt tournamentIds
        res <- Test.eval $ toTop $ S.getGames requestData
        Test.shouldEqual (L.length res) (L.length gamesForDB)


-- Setting up the database fixtures. This function is time-intensive, and run
-- once before a set of tests is executed. These tests do not modify the data.
-- Thus we want to set up this function that re-running it doesn't delete and
-- re-insert the data, which shortens the time for running the tests

doIO :: IO ()
doIO = do
  let dbName = "test"
  dbDatabases :: [Entity Database] <- inBackendTest $ selectList [] []

  let alreadyRun = False -- length dbDatabases > 0
  let runEval = True
  let onlyContinueEval = False
  let settings = Fix.FixtureSettings dbName runEval onlyContinueEval
  unless alreadyRun $ Fix.runJob settings
  return ()

defaultDBName = "dummy games"

getDefaultDBId = fmap (P.entityKey . L.head) $ liftIO $ inBackendTest $ selectList [(P.==.) DatabaseName defaultDBName] []

expectedTest text expected parse = do
  let result = parseOnly parse $ pack text
  result `shouldBe` expected


playerGameData dbName = do
  defaultDBId <- getDefaultDBId
  players :: [Entity Player] <- liftIO $ inBackendTest  $ selectList [(P.==.) PlayerDatabaseId defaultDBId] []
  dbGames :: [Entity Game] <- liftIO $ inBackendTest $ selectList [(P.==.) GameDatabaseId defaultDBId] []
  return (defaultDBId, players, dbGames)

testOpening :: Spec
testOpening = describe "The opening module" $ do

  it "can correctly parse the code" $ 
    expectedTest "A00" (Right "A00") openingCodeParser

  it "can parse a simple opening name" $ 
    expectedTest "Test' Opening" (Right "Test' Opening") openingNameParser

  it "can parse a incorrect opening name" $ 
    expectedTest "Test' Opening\n" (Right "Test' Opening") openingNameParser

  it "can parse an opening name with comments" $ 
    expectedTest "Test' Opening; comments" (Right "Test' Opening") openingNameParser

  it "can parse a game move" $ 
    expectedTest "1. e4 1/2" (Right "1. e4") openMoveParser

  it "can parse the game list" $ 
    expectedTest "A00 A\n1.a3 1/2" (Right (ListData "A00" "A" "1.a3")) parseListData

  it "can parse the game list with newlines" $ 
    expectedTest "A00 A\n1.a3 1/2\n\n\n" (Right (ListData "A00" "A" "1.a3")) parseListData

getTimeString :: IO String
getTimeString = fmap show getTimeInt

getTimeInt :: IO Int
getTimeInt = getCurrentTime >>= pure . (1000*) . utcTimeToPOSIXSeconds >>= pure . round

getParams :: IO [(B.ByteString, B.ByteString)]
getParams = do
  userName :: String <- liftIO getTimeString
  return [("login", B.pack userName), ("password", "password")]

loginForApi userName = toTop $ do
  S.createFullUser userName "password"
  S.forceLoginFromEmail userName

instance Test.HasSession App.App where
  getSessionLens = App.sess

loginModifier userName h = do
  loginForApi userName
  h

testUserHandling :: Spec
testUserHandling = Test.snap (route (App.routes True)) (App.app settings) $ do
  describe "In the user handling ," $ do
    it "getting user name works" $ do
      userName :: String <- liftIO getTimeString
      res <- Test.eval $ do
        loginForApi userName
        toTop S.currentUserName
      Test.shouldEqual res (Just userName)

    it "running createFullUser adds an AppUser" $ do
      userName :: String <- liftIO getTimeString
      dbUsersBefore :: [Entity AppUser] <- liftIO $ inBackendTest  $ selectList [] []
      Test.eval $ toTop $ S.createFullUser userName "password"
      dbUsersAfter :: [Entity AppUser] <- liftIO $ inBackendTest $ selectList [] []
      Test.shouldEqual ((length dbUsersBefore) + 1) (length dbUsersAfter)

    it "changing user works" $ do
      userName :: String <- liftIO getTimeString
      let userName2 = userName ++ "test"

      runUser <- Test.eval $ do
        loginForApi userName
        loginForApi userName2
        toTop S.currentUserName

      Test.shouldEqual (Just userName2) runUser
