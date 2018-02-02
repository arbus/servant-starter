module Lib.Server where

import Lib.Prelude
import Servant
import Servant.Server
import Data.Map as Map
import Network.Wai

type API = "hello" :> Get '[JSON] Text

mkAppEnv :: IO AppEnv
mkAppEnv = do
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