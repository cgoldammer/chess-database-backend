{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Main where

import Data.Maybe
import Control.Comonad
import qualified Data.ByteString.Char8 as B
import Control.Monad.IO.Class (liftIO)
import           Data.IORef
import Snap.Snaplet.PostgresqlSimple
import           Control.Monad.State
import Control.Lens (view)
import qualified Services.Service as S
import Data.Either
import           Snap.Snaplet.Config
import           Snap.Http.Server
import           Snap.Snaplet
import qualified System.Environment as Env

import           Application

defaultDB = "test"

getDBFromEnvironment :: IO String
getDBFromEnvironment = do
  envDB <- Env.lookupEnv "dbName"
  return $ maybe defaultDB id envDB


main :: IO ()
main = do
  dbName <- getDBFromEnvironment
  serveSnaplet defaultConfig $ app dbName

