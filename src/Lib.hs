{-# LANGUAGE RecordWildCards #-}
module Lib where

import           Lib.Prelude
import           Lib.Server
import           Network.Wai.Handler.Warp
import           Servant.Server           (serve)
import           System.Remote.Monitoring as EKG

libMain :: IO ()
libMain = do
    env@AppEnv{..} <- mkAppEnv
    _ <- EKG.forkServerWith ekgStore "localhost" 8000
    run 8080 (mkServer env)
