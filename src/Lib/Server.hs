{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators    #-}
module Lib.Server where

import           Data.IORef
import           Data.Map                   as Map
import           Data.Pool
import           Database.PostgreSQL.Simple
import           Lib.Prelude
import           Network.Wai
import           Servant
import           Servant.Server
import           System.Environment
import           System.Metrics             as Metrics

type API = "hello" :> Get '[JSON] Text

mkAppEnv :: IO AppEnv
mkAppEnv = do
    dbUrl <- fromMaybe "host=localhost port=5432 dbname=test" <$> lookupEnv "DATABASE_URL"
    dbPool <- createPool (connectPostgreSQL $ toS dbUrl) close 10 5 10
    sessions <- newMVar Map.empty
    sqlDistributions <- newIORef Map.empty
    ekgStore <- Metrics.newStore
    Metrics.registerGcMetrics ekgStore
    return AppEnv{..}

server :: AppEnv -> Server API
server env =
    enter (runAppT env) server'
    where
    server' :: ServerT API App
    server' = return "world"

mkServer :: AppEnv -> Application
mkServer env = serve (Proxy @API) (server env)
