{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module Application
  ( App
  , app
  , routes
  , auth
  , service
  , sess
  ) where

import AppTypes
import Control.Lens (makeLenses, view)
import Control.Monad (join, liftM3, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State.Class (get)
import qualified Data.ByteString.Char8 as B (ByteString, pack, unpack)
import Data.Map (Map)
import qualified Data.Map as Map (lookup)
import Data.Maybe (fromMaybe, listToMaybe)
import qualified Data.Text as T (Text, pack, unpack)
import Database.Persist.Sql (runMigrationUnsafe)
import Debug.Trace (trace)
import qualified Services.Service as S
import Snap.Core
  ( Method(..)
  , getRequest
  , method
  , modifyResponse
  , redirect
  , rqParams
  , setResponseStatus
  , writeBS
  )
import Snap.Snaplet
  ( Handler
  , Snaplet
  , SnapletInit
  , addRoutes
  , makeSnaplet
  , nestSnaplet
  , snapletValue
  , subSnaplet
  , with
  , withTop
  )
import Snap.Snaplet.Auth
  ( AuthFailure
  , AuthManager(..)
  , AuthUser
  , clearPasswordResetToken
  , currentUser
  , loginUser
  , logout
  , lookupByLogin
  , registerUser
  , save
  , setPassword
  , setPasswordResetToken
  , userLogin
  , userResetToken
  )
import Snap.Snaplet.Auth.Backends.Persistent
  ( initPersistAuthManager
  , migrateAuth
  )
import Snap.Snaplet.Heist (HasHeist, Heist, heistInit, heistLens)
import Snap.Snaplet.Persistent (PersistState, persistPool)
import Snap.Snaplet.Session (SessionManager)
import Snap.Snaplet.Session.Backends.CookieSession (initCookieSessionManager)

data App = App
  { _heist :: Snaplet (Heist App)
  , _sess :: Snaplet SessionManager
  , _db :: Snaplet PersistState
  , _auth :: Snaplet (AuthManager App)
  , _service :: Snaplet (S.Service App)
  }

makeLenses ''App

instance HasHeist App where
  heistLens = subSnaplet heist

app :: Settings -> SnapletInit App App
app settings =
  makeSnaplet "app" "An snaplet example application." Nothing $ do
    let dbName = appDBName settings
    h <- nestSnaplet "" heist $ heistInit "templates"
    s <-
      nestSnaplet "sess" sess $
      initCookieSessionManager "site_key.txt" "sess" Nothing (Just 3600)
    d <-
      nestSnaplet "db" db $
      S.initPersistWithDB dbName (runMigrationUnsafe migrateAuth)
    a :: Snaplet (AuthManager App) <-
      nestSnaplet "auth" auth $
      initPersistAuthManager sess (persistPool $ view snapletValue d)

    let user = view snapletValue a
    let login = T.unpack . userLogin <$> activeUser user

    serviceSnaplet <- nestSnaplet "api" service $ S.serviceInit dbName auth
    addRoutes $ routes $ showLogin settings
    return $ App h s d a serviceSnaplet

routes :: Bool -> [(B.ByteString, Handler App App ())]
routes False = []
routes True = routes False ++ loginRoutes

loginRoutes :: [(B.ByteString, Handler App App ())]
loginRoutes =
  [ ("login", with auth handleLoginSubmit)
  , ("register", with auth handleNewUser)
  , ("logout", with auth handleLogout >> resetUser)
  , ("resetPasswordData", with auth resetPasswordHandler)
  -- disabled until I think through how to avoid spamming
  -- , ("sendPasswordResetEmail", with auth sendPasswordResetHandler)
  ]

writeLoginSuccess :: Handler b (AuthManager b) ()
writeLoginSuccess = do
  user <- currentUser
  let login = fmap (T.unpack . userLogin) user :: Maybe String
  modifyResponse $ setResponseStatus 200 "Success"
  writeBS $ B.pack $ fromMaybe "" login

writeLoginFailure :: AuthFailure -> Handler b (AuthManager b) ()
writeLoginFailure failure = do
  modifyResponse $ setResponseStatus 403 "Login failed"
  writeBS $ B.pack $ show failure

handleLoginSubmit :: Handler App (AuthManager App) ()
handleLoginSubmit = do
  loginUser "email" "password" Nothing writeLoginFailure writeLoginSuccess
  user <- currentUser
  let login = fmap (T.unpack . userLogin) user
  liftIO $ print $ "Changing user to" ++ show login
  return ()

resetUser :: Handler App App ()
resetUser = do
  withTop auth logout
  return ()

handleLogout :: Handler App (AuthManager App) ()
handleLogout = logout

registerNew :: Handler App (AuthManager App) (Either AuthFailure AuthUser)
registerNew = method POST $ registerUser "email" "password"

handleNewUser :: Handler App (AuthManager App) ()
handleNewUser = do
  res <- registerNew
    -- Registering creates a `snap_auth_user` in the database. However, we
    -- also want to create an `app_user` that is linked to the `snap_auth_user`,
    -- because this allows us to assume a one-to-one relationship between
    -- the tables
  trace (show res) $
    case res of
      Right authUser -> do
        let usId = userLogin authUser
        withTop service $ S.createAppUser usId
        handleLoginSubmit
        writeLoginSuccess
      Left authFail -> writeLoginFailure authFail

-- Logic for resetting passwords. This works as follows:
-- 1. A request is sent to '/sendPasswordResetEmail?email=example@example.com'
-- 2. The backend creates a password reset token for the user
-- 3. The backend sends out an email linking to 
-- '/resetpassword?email...&token=..."
-- 4. The form collects a new password and submits it 
-- to "/resetPasswordData?email...&token=...&password=..."
-- 5. The password is reset, the token for the user is destroyed
-- 6. The server forwards to /passwordgood
-- The endpoints "/resetpassword" and "/passwordgood" are not defined here
-- but are automatically created from the corresponding heist templates.
getProperty :: String -> Map B.ByteString [B.ByteString] -> Maybe T.Text
getProperty name queryMap =
  fmap (T.pack . B.unpack) $
  listToMaybe =<< Map.lookup (B.pack name) queryMap

resetWithUser :: T.Text -> Handler b (AuthManager b) ()
resetWithUser login = do
  request <- getRequest
  let params = rqParams request
  let token = getProperty "token" params
  manager <- get
  let getUser AuthManager {backend = b} = lookupByLogin b login
  maybeUser <- liftIO $ getUser manager
  let newPass = getProperty "password" params
  let resetter = liftM3 (resetPassForUser manager) token maybeUser newPass
  fromMaybe (return ()) resetter
  clearPasswordResetToken login
  return ()

resetPassForUser ::
     AuthManager b
  -> T.Text
  -> AuthUser
  -> T.Text
  -> Handler b (AuthManager b) ()
resetPassForUser manager token user newPass = do
  let storedToken = userResetToken user
  when (storedToken == Just token) $ do
    updatedUser <- liftIO $ setPassword user $ B.pack $ T.unpack newPass
    liftIO $ save manager updatedUser
    let login = userLogin user
    clearPasswordResetToken login
    redirect "/snap_prod/passwordchangegood"

resetPasswordHandler :: Handler b (AuthManager b) ()
resetPasswordHandler = do
  request <- getRequest
  let params = rqParams request
  let user = getProperty "email" params
  maybe (return ()) resetWithUser user
  return ()

sendPasswordResetHandler :: Handler b (AuthManager b) ()
sendPasswordResetHandler = do
  request <- getRequest
  let params = rqParams request
  let user = T.unpack <$> getProperty "email" params
  maybe (return ()) sendPasswordResetEmail user

sendPasswordResetEmail :: String -> Handler b (AuthManager b) ()
sendPasswordResetEmail email = do
  token <- setPasswordResetToken (T.pack email)
  maybe (return ()) (sendEmailForToken email) token

sendEmailForToken :: String -> T.Text -> Handler b (AuthManager b) ()
sendEmailForToken email token = do
  let url = "https://chessinsights.org/snap_prod/resetpassword?"
  let fullUrl = url ++ "email=" ++ email ++ "&token=" ++ T.unpack token
  let body = "Reset password link for chess insights \n " ++ fullUrl
  liftIO $ S.trySendEmail "Password reset for chessinsights.org" email body
