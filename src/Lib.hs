module Lib where

import           Lib.Prelude
import Lib.Server
import           Network.Wai.Handler.Warp
import Servant.Server(serve)
import Data.Map as Map

libMain :: IO ()
libMain = do
    sessions <- newMVar Map.empty
    run 8080 $ serve (Proxy @API) (server AppEnv{..})
