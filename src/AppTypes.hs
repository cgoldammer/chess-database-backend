module AppTypes where

data AppType = Dev | Prod | Test deriving Show
data Settings = Settings {
  appType :: AppType
, showLogin :: Bool
, appDBName :: String
, appPort :: Int
} deriving Show

getSettings :: AppType -> Settings
getSettings Dev = Settings Dev True (getDBName Dev) (getPortForApp Dev)
getSettings Prod = Settings Prod False (getDBName Prod) (getPortForApp Prod)
getSettings Test = Settings Test True (getDBName Test) (getPortForApp Test)

getAppType :: String -> AppType
getAppType "prod" = Prod
getAppType "test" = Test
getAppType _ = Dev

getDBName :: AppType -> String
getDBName Dev = "dev"
getDBName Test = "test"
getDBName Prod = "prod"

getPortForApp :: AppType -> Int
getPortForApp Dev = 8000
getPortForApp Prod = 8001
getPortForApp Test = 8002
