{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Main where

import Data.Maybe
import Control.Comonad
import           Control.Applicative
import           Snap.Core
import           Snap.Util.FileServe
import           Snap.Http.Server
import           Application
import           Snap.Snaplet.Config
import           Snap.Snaplet
import           Snap.Snaplet.Auth
import           Snap.Snaplet.Auth.Backends.JsonFile
import           Snap.Snaplet.Heist
import           Snap.Snaplet.Session.Backends.CookieSession
import           Snap.Util.FileServe
import qualified Data.ByteString.Char8 as B
import Control.Monad.IO.Class (liftIO)
import           Data.IORef
import Snap.Snaplet.PostgresqlSimple
import           Control.Monad.State
import Control.Lens (view)
import qualified Heist.Interpreted as I
import qualified Data.Text as T
import           Data.Map.Syntax ((##))
import Database.Persist as P
import Database.Persist.Sql
import Snap.Snaplet.Persistent
import           Snap.Snaplet.Session
import           Snap.Snaplet.Session.Backends.CookieSession
import           Snap.Snaplet.Auth.Backends.Persistent
import qualified Services.Service as S
import Data.Either


main :: IO ()
main = serveSnaplet defaultConfig app

app :: SnapletInit App App
app = makeSnaplet "app" "An snaplet example application." Nothing $ do
    h <- nestSnaplet "" heist $ heistInit "templates"
    s <- nestSnaplet "sess" sess $ initCookieSessionManager "site_key.txt" "sess" Nothing (Just 3600)
    d <- nestSnaplet "db" db $ initPersist (runMigrationUnsafe migrateAuth)
    a <- nestSnaplet "auth" auth $ initPersistAuthManager sess (persistPool $ view snapletValue d)
    ls <- nestSnaplet "levels" service $ S.serviceInit auth
    addRoutes routes
    return $ App h s d a ls

routes = [
    ("test", writeBS "hi you"),
    ("register", with auth handleNewUser),
    ("logout", with auth handleLogout),
    ("login", with auth handleLoginSubmit)
    ]

handleLogins :: T.Text -> Handler App (AuthManager App) ()
handleLogins authError = heistLocal (I.bindSplices errs) $ render "login_results"
  where
      errs = "loginError" ## I.textSplice authError

handleLoginSubmit :: Handler App (AuthManager App) ()
handleLoginSubmit = loginUser "login" "password" Nothing (\err -> handleLogins ((T.pack . show) err)) (handleLogins "")

handleLogout :: Handler App (AuthManager App) ()
handleLogout = logout -- >> redirect "/"


registerEvent (Left x) = T.pack $ show x
registerEvent (Right x) = T.pack ""

handleNewUser :: Handler App (AuthManager App) ()
handleNewUser = do 
    -- redirect "test"
    res :: Either AuthFailure AuthUser <- method POST $ registerUser "login" "password" <|> method GET (redirect "/test")
    -- Registering creates a `snap_auth_user` in the database. However, we
    -- also want to create a `level_user` that is linked to the `snap_auth_user`,
    -- because this allows us to assume a one-to-one relationship between
    -- the tables
    -- case res of
    --     Right authUser -> do
    --             let loginText = userLogin authUser
    --             withTop levelService $ S.createUser loginText
    --             handleLoginSubmit
    --     Left _ -> do
    --             return ()

    -- handleLogins (registerEvent res)
    return ()
