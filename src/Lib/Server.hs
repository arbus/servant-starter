module Lib.Server where

import Lib.Prelude
import Servant
import Servant.Server

type API = "hello" :> Get '[JSON] Text

server :: Server API
server = return "world"
