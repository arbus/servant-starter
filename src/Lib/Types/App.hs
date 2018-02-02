module Lib.Types.App where

import           Data.Pool
import           Database.PostgreSQL.Simple
import           Lib.Types.Session
import           Protolude

data AppError =
      Invalid Text
    | NotAllowed Text
    | NotFound
    | ServerError Text

data AppEnv = AppEnv {
    dbPool   :: Pool Connection,
    sessions :: MVar (Map Text Session)
}

newtype App a = App {
    runApp :: ReaderT AppEnv (ExceptT AppError IO) a
} deriving (Monad, Functor, Applicative, MonadReader AppEnv, MonadError AppError, MonadIO)
