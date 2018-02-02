module Lib where

import           Lib.Prelude
import Lib.Server
import           Network.Wai.Handler.Warp
import Servant.Server(serve)

libMain :: IO ()
libMain = run 8080 $ serve (Proxy @API) server
