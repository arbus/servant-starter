module Lib.Util.App where

import Protolude
import Lib.Types
import Servant.Server
import Database.PostgreSQL.Simple as PG
import Data.Pool

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

appMaybeWith :: AppError -> App (Maybe a) -> App a
appMaybeWith err x = do
    res <- x
    case res of
        Just y  -> return y
        Nothing -> throwError err

appMaybe :: App (Maybe a) -> App a
appMaybe = appMaybeWith NotFound

queryPG :: (PG.ToRow q, PG.FromRow r) => PG.Query -> q -> App [r]
queryPG q args =
    liftIO . flip withResource (\conn -> PG.query conn q args) =<< asks dbPool

queryPG_ :: (PG.FromRow r) => PG.Query -> App [r]
queryPG_ q =
    liftIO . flip withResource (`PG.query_` q) =<< asks dbPool

executePG :: (PG.ToRow q) => PG.Query -> q -> App ()
executePG q args =
    liftIO . flip withResource (\conn -> void $ PG.execute conn q args) =<< asks dbPool

executePG_ :: PG.Query -> App ()
executePG_ q =
    liftIO . flip withResource (\conn -> void $ PG.execute_ conn q) =<< asks dbPool

executeManyPG :: (PG.ToRow q) => PG.Query -> [q] -> App ()
executeManyPG q args =
    liftIO . flip withResource (\conn -> void $ PG.executeMany conn q args) =<< asks dbPool

-- helper function for when you are expecting only one
-- row to be returned from an app action, typically db requests
asSingleRow :: App [a] -> App a
asSingleRow = appMaybe . (head <$>)