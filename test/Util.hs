module Util where

import           Lib.Server
import           Network.HTTP.Client
import           Network.Wai.Handler.Warp
import           Protolude
import           Servant.Client

type Host = (Manager, BaseUrl)

withApp :: (Host -> IO a) -> IO a
withApp action = do
  appEnv <- mkAppEnv
  testWithApplication (return $ mkServer appEnv) $ \ port -> do
    manager <- newManager defaultManagerSettings
    let url = BaseUrl Http "localhost" port ""
    action (manager, url)
