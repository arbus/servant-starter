module Lib.Types.App where

import           Data.IORef
import           Data.Map                    as Map
import           Data.Pool
import           Database.PostgreSQL.Simple
import           Lib.Types.Session
import           Protolude
import           System.Metrics              as Metrics
import           System.Metrics.Distribution as Dist

data AppError =
      Invalid Text
    | NotAllowed Text
    | NotFound
    | ServerError Text

data AppEnv = AppEnv {
    dbPool           :: Pool Connection,
    sessions         :: MVar (Map Text Session),
    sqlDistributions :: IORef (Map.Map Text Dist.Distribution),
    ekgStore         :: Metrics.Store
}

newtype App a = App {
    runApp :: ReaderT AppEnv (ExceptT AppError IO) a
} deriving (Monad, Functor, Applicative, MonadReader AppEnv, MonadError AppError, MonadIO)
