{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

------------------------------------------------------------------------------
-- | This module defines our application's state type and an alias for its
-- handler monad.
module Application where

------------------------------------------------------------------------------
import Control.Lens
import Control.Monad.IO.Class (MonadIO, liftIO)
import Snap.Snaplet
import Snap.Snaplet.Heist
import Snap.Snaplet.Session
import qualified Data.ByteString.Char8 as B
import           Data.IORef
import Snap.Snaplet.Persistent
import qualified Data.Text as T
import qualified Heist.Interpreted as I
import           Data.Map.Syntax ((##))
import Database.Persist as P
import Database.Persist.Sql
import Snap.Snaplet.Persistent
import           Snap.Snaplet.Session
import           Snap.Snaplet.Session.Backends.CookieSession
import           Snap.Snaplet.Auth.Backends.Persistent
import           Control.Applicative
import           Snap.Core
import           Snap.Util.FileServe
import           Snap.Http.Server
import           Snap.Snaplet.Config
import           Snap.Snaplet
import           Snap.Snaplet.Auth
import           Snap.Snaplet.Auth.Backends.JsonFile
import           Snap.Snaplet.Heist
import           Snap.Snaplet.Session.Backends.CookieSession
import           Snap.Util.FileServe
import Debug.Trace

import qualified Services.Service as S

------------------------------------------------------------------------------
data App = App
    { _heist :: Snaplet (Heist App)
    , _sess :: Snaplet SessionManager
    , _db :: Snaplet PersistState
    , _auth :: Snaplet (AuthManager App)
    , _service :: Snaplet S.Service
    }

makeLenses ''App

instance HasHeist App where
    heistLens = subSnaplet heist

type AppHandler = Handler App App


app :: String -> SnapletInit App App
app dbName = makeSnaplet "app" "An snaplet example application." Nothing $ do
    h <- nestSnaplet "" heist $ heistInit "templates"
    s <- nestSnaplet "sess" sess $ initCookieSessionManager "site_key.txt" "sess" Nothing (Just 3600)
    d <- nestSnaplet "db" db $ S.initPersistWithDB dbName (runMigrationUnsafe migrateAuth)
    a <- nestSnaplet "auth" auth $ initPersistAuthManager sess (persistPool $ view snapletValue d)
    ls <- nestSnaplet "api" service $ S.serviceInit dbName
    addRoutes routes
    return $ App h s d a ls

routes = [
    ("test", writeBS "hi you"),
    ("fail", writeBS "lgin error"),
    ("login", with auth handleLoginSubmit),
    ("register", with auth handleNewUser),
    ("logout", with auth handleLogout >> resetUser)
    ]

handleLogins :: T.Text -> Handler App (AuthManager App) ()
handleLogins authError = heistLocal (I.bindSplices errs) $ render "login_results"
  where
      errs = "loginError" ## I.textSplice authError


nothingHandler :: Handler App (AuthManager App) ()
nothingHandler = do
  return ()

handleLoginSubmit :: Handler App (AuthManager App) ()
handleLoginSubmit = do
  us <- loginUser "email" "password" Nothing (\err -> writeBS (B.pack ("Error: " ++ show err))) nothingHandler
  user <- currentUser
  let login = fmap (T.unpack . userLogin) user
  withTop service $ S.changeUser login
  return ()
-- handleLoginSubmit = loginUser "login" "password" Nothing (\err -> handleLogins ((T.pack . show) err)) (redirect "/test")

resetUser :: Handler App App ()
resetUser = do
  with service $ S.changeUser Nothing
  return ()

handleLogout :: Handler App (AuthManager App) ()
handleLogout = logout

registerEvent (Left x) = T.pack $ show x
registerEvent (Right x) = T.pack ""

registerNew :: Handler App (AuthManager App) (Either AuthFailure AuthUser) 
registerNew = method POST $ registerUser "email" "password"

handleNewUser :: Handler App (AuthManager App) ()
handleNewUser = do 
    -- redirect "test"
    res <- registerNew
    -- Registering creates a `snap_auth_user` in the database. However, we
    -- also want to create an `app_user` that is linked to the `snap_auth_user`,
    -- because this allows us to assume a one-to-one relationship between
    -- the tables
    trace (show res) $ case res of
        Right authUser -> do
                let usId = userLogin authUser
                withTop service $ S.createAppUser usId
                return ()
        Left _ -> do
                return ()

    handleLoginSubmit
    return ()
