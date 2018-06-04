{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, TypeFamilies #-}

module Main where

import Services.Helpers
import Services.Service
import qualified Test.Helpers as TH
import Database.Esqueleto hiding (get)
import Services.DatabaseHelpers
import Database.Persist hiding ((==.))
import Database.Persist.Sql hiding ((==.))

import Services.Types


main :: IO ()
main = do
  (players, evals) <- TH.inBackend (connString dbName) dataResults
  let summ = summarizeEvals players evals
  print $ length summ

getTestEvals :: TH.DataAction [EvalResult]
getTestEvals = do
  er <- select $
    from $ \(me, g) -> do
      where_ $ (me^.MoveEvalGameId ==. g^.GameId)
      return (me, g)
  return er

dbName :: String
dbName = "prod"

dataResults :: TH.DataAction ([Entity Player], [EvalResult])
dataResults = do
  dbPlayers :: [Entity Player] <- selectList [] []
  evals <- getTestEvals
  return (dbPlayers, evals)
