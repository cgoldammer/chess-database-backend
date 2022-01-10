{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Comonad
import Control.Lens (view)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State
import qualified Data.ByteString.Char8 as B
import Data.Either
import Data.IORef
import Data.Maybe
import qualified Services.Service as S
import Snap.Http.Server
import Snap.Snaplet
import Snap.Snaplet.Config
import Snap.Snaplet.PostgresqlSimple
import System.Environment (lookupEnv)

import AppTypes
import Application

readSettings :: IO Settings
readSettings = do
  appTypeEnv :: Maybe String <- liftIO $ lookupEnv "type"
  let appType_ = maybe Dev getAppType appTypeEnv
  return $ getSettings appType_

main :: IO ()
main = do
  settings <- readSettings
  print settings
  let config = setErrorLog (ConfigFileLog "error.txt") $ setAccessLog (ConfigFileLog "log.txt") $ setPort (appPort settings) defaultConfig
  serveSnaplet config $ app settings

