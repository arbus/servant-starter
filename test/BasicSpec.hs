module BasicSpec where

import Protolude
import Servant.Client
import Lib.Server
import Test.Hspec
import Util

helloEndpoint :: ClientM Text
helloEndpoint = client (Proxy @API)

spec :: Spec
spec =
    describe "GET endpoints" $ around withApp $ do
        it "should say world" $ \ (manager, baseUrl) ->
            runClientM helloEndpoint (ClientEnv manager baseUrl) `shouldReturn` Right "world"