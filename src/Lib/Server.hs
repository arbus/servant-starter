module Lib.Server where

import Lib.Prelude
import Servant
import Servant.Server
import Data.Map as Map
import Network.Wai
import System.Environment
import Data.Pool
import           Database.PostgreSQL.Simple

type API = "hello" :> Get '[JSON] Text

mkAppEnv :: IO AppEnv
mkAppEnv = do
    dbUrl <- fromMaybe "host=localhost port=5432 dbname=test" <$> lookupEnv "DATABASE_URL"
    dbPool <- createPool (connectPostgreSQL $ toS dbUrl) close 10 5 10
    sessions <- newMVar Map.empty
    return AppEnv{..}

server :: AppEnv -> Server API
server env =
    enter (runAppT env) server'
    where
    server' :: ServerT API App
    server' = return "world"

mkServer :: AppEnv -> Application
mkServer env = serve (Proxy @API) (server env)