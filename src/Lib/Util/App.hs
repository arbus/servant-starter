{-# LANGUAGE BangPatterns  #-}
{-# LANGUAGE TypeOperators #-}
module Lib.Util.App where

import           Data.IORef
import           Data.Map                    as Map
import           Data.Pool
import           Database.PostgreSQL.Simple  as PG
import           Lib.Types
import           Protolude
import           Servant.Server
import           System.CPUTime
import           System.Metrics              as Metrics
import           System.Metrics.Distribution as Dist

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

timedAction :: Text -> App a -> App a
timedAction actionName action = do
    start <- liftIO getCPUTime
    result <- action
    end <- liftIO getCPUTime
    let !timeTaken = fromIntegral (end - start) * 1e-12
    dist <- getOrCreateDistribution actionName
    liftIO $ Dist.add dist timeTaken
    return result

queryPG :: (PG.ToRow q, PG.FromRow r) => Text -> PG.Query -> q -> App [r]
queryPG queryName q args = timedAction queryName $
    liftIO . flip withResource (\conn -> PG.query conn q args) =<< asks dbPool

queryPG_ :: (PG.FromRow r) => Text -> PG.Query -> App [r]
queryPG_ queryName q = timedAction queryName $
    liftIO . flip withResource (`PG.query_` q) =<< asks dbPool

executePG :: (PG.ToRow q) => Text -> PG.Query -> q -> App ()
executePG queryName q args = timedAction queryName $
    liftIO . flip withResource (\conn -> void $ PG.execute conn q args) =<< asks dbPool

executePG_ :: Text -> PG.Query -> App ()
executePG_ queryName q = timedAction queryName $
    liftIO . flip withResource (\conn -> void $ PG.execute_ conn q) =<< asks dbPool

executeManyPG :: (PG.ToRow q) => Text -> PG.Query -> [q] -> App ()
executeManyPG queryName q args = timedAction queryName $
    liftIO . flip withResource (\conn -> void $ PG.executeMany conn q args) =<< asks dbPool

-- helper function for when you are expecting only one
-- row to be returned from an app action, typically db requests
asSingleRow :: App [a] -> App a
asSingleRow = appMaybe . (head <$>)

getSession :: Text -> App (Maybe Session)
getSession sessionKey = do
    sessionsVar <- asks sessions
    sessionMap <- liftIO $ readMVar sessionsVar
    return Map.lookup sessionKey sessionMap

putSession :: Text -> Session -> App ()
putSession sessionKey newSession = do
    sessionsVar <- asks sessions
    liftIO $ modifyMVar_ sessionsVar (Map.insert sessionKey newSession)

deleteSession :: Text -> App ()
deleteSession sessionKey = do
    sessionsVar <- asks sessions
    liftIO $ modifyMVar_ sessionsVar (Map.delete sessionKey)

getOrCreateDistribution :: Text -> App Dist.Distribution
getOrCreateDistribution name = do
    ref <- asks sqlDistributions
    store <- asks ekgStore
    liftIO $ do
        distMap <- readIORef ref
        case Map.lookup name distMap of
            Just dist -> return dist
            Nothing -> do
                newDist <- Metrics.createDistribution name store
                modifyIORef' ref (Map.insert name newDist)
                return newDist
