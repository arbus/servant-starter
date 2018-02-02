module Lib where

import           Lib.Prelude
import           Lib.Server
import           Network.Wai.Handler.Warp
import           Servant.Server           (serve)

libMain :: IO ()
libMain = do
    env <- mkAppEnv
    run 8080 (mkServer env)
