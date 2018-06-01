{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

------------------------------------------------------------------------------
-- | This module defines our application's state type and an alias for its
-- handler monad.
module Application (App, app, routes, auth) where

------------------------------------------------------------------------------
import Control.Lens (makeLenses, view)
import Snap.Snaplet (Snaplet, Handler, SnapletInit, subSnaplet, makeSnaplet, nestSnaplet, snapletValue, addRoutes, with, withTop)
import Snap.Snaplet.Session (SessionManager)
import qualified Data.ByteString.Char8 as B (ByteString, pack)
import Snap.Snaplet.Persistent (PersistState, persistPool)
import qualified Data.Text as T (unpack)
import Database.Persist.Sql (runMigrationUnsafe)
import Snap.Snaplet.Auth.Backends.Persistent (migrateAuth, initPersistAuthManager)
import Snap.Core (writeBS, method, Method(..))
import Snap.Snaplet.Auth (AuthManager, AuthFailure, AuthUser, loginUser, currentUser, userLogin, logout, registerUser)
import Snap.Snaplet.Heist (Heist, HasHeist, heistLens, heistInit)
import Snap.Snaplet.Session.Backends.CookieSession (initCookieSessionManager)
import Debug.Trace (trace)

import qualified Services.Service as S
import AppTypes

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

app :: Settings -> SnapletInit App App
app settings = makeSnaplet "app" "An snaplet example application." Nothing $ do
  let dbName = appDBName settings
  h <- nestSnaplet "" heist $ heistInit "templates"
  s <- nestSnaplet "sess" sess $ initCookieSessionManager "site_key.txt" "sess" Nothing (Just 3600)
  d <- nestSnaplet "db" db $ S.initPersistWithDB dbName (runMigrationUnsafe migrateAuth)
  a <- nestSnaplet "auth" auth $ initPersistAuthManager sess (persistPool $ view snapletValue d)
  ls <- nestSnaplet "api" service $ S.serviceInit dbName

  addRoutes $ routes $ showLogin settings
  return $ App h s d a ls

routes :: Bool -> [(B.ByteString, Handler App App ())]
routes False = []
routes True = routes False ++ loginRoutes

loginRoutes :: [(B.ByteString, Handler App App ())]
loginRoutes = [
    ("login", with auth handleLoginSubmit),
    ("register", with auth handleNewUser),
    ("logout", with auth handleLogout >> resetUser)
    ]

nothingHandler :: Handler App (AuthManager App) ()
nothingHandler = do
  return ()

handleLoginSubmit :: Handler App (AuthManager App) ()
handleLoginSubmit = do
  loginUser "email" "password" Nothing (\err -> writeBS (B.pack ("Error: " ++ show err))) nothingHandler
  user <- currentUser
  let login = fmap (T.unpack . userLogin) user
  withTop service $ S.changeUser login
  return ()

resetUser :: Handler App App ()
resetUser = do
  with service $ S.changeUser Nothing
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
    trace (show res) $ case res of
        Right authUser -> do
                let usId = userLogin authUser
                withTop service $ S.createAppUser usId
                return ()
        Left _ -> do
                return ()

    handleLoginSubmit
    return ()
