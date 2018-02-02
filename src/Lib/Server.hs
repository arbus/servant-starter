module Lib.Server where

import Lib.Prelude
import Servant
import Servant.Server

type API = "hello" :> Get '[JSON] Text

server :: AppEnv -> Server API
server env =
    enter (runAppT env) server'
    where
    server' :: ServerT API App
    server' = return "world"

