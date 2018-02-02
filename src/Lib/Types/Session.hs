module Lib.Types.Session where

import           Protolude

newtype Session = Session {
    isLoggedIn :: Bool
}
