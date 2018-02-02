module Lib.Util.App where

import Protolude
import Lib.Types
import Servant.Server

runAppT :: AppEnv -> (App :~> Handler)
runAppT env = NT (runAppT' env)
    where
    runAppT' :: AppEnv -> App a -> Handler a
    runAppT' env action = do
        res <- liftIO $ runExceptT $ runReaderT (runApp action) env
        case res of
            Left (Invalid text)     -> throwError $ err400 { errBody = toSL text }
            Left NotFound           -> throwError err404
            Left (NotAllowed text)  -> throwError $ err401 { errBody = toSL text }
            Left (ServerError text) -> throwError $ err500 { errBody = toSL text }
            Right a                 -> return a