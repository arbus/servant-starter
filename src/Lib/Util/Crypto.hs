module Lib.Util.Crypto where

import           Crypto.BCrypt
import           Data.Aeson
import qualified Data.Map              as Map
import           Data.Time.Calendar
import           Data.Time.Clock.POSIX
import           Lib.Types
import           Lib.Util.App          (appMaybeWith)
import           Protolude
import           System.Random
import           Web.JWT

type PasswordHash = Text
type PasswordPlainText = Text

mkPasswordHash :: PasswordPlainText -> App PasswordHash
mkPasswordHash password =
  let hash = hashPasswordUsingPolicy slowerBcryptHashingPolicy $ toS password
      hashText = (toS <$>) <$> hash
  in appMaybeWith (ServerError "Error generating password hash") $ liftIO hashText

checkPassword :: PasswordPlainText -> PasswordHash -> Bool
checkPassword password hash = validatePassword (toS hash) (toS password)

mkRandomString :: (MonadIO m) => Int -> m Text
mkRandomString len = do
  gen <- liftIO newStdGen
  return $ toS $ take len $ randomRs ('a', 'z') gen

mkJWTToken :: Int -> Map.Map Text Value -> App Text
mkJWTToken expiryInSeconds customClaims = do
  secretKey <- secret <$> asks jwtSecret
  timeNow <- liftIO getPOSIXTime
  let expiryTime = timeNow + fromIntegral expiryInSeconds
  let
    cs = def { -- def returns a default JWTClaimsSet
      Web.JWT.exp = numericDate expiryTime
      , unregisteredClaims = customClaims
    }
  return $ encodeSigned HS256 secretKey cs

decodeAndVerifyJWTToken :: Text -> App (Maybe (Map.Map Text Value))
decodeAndVerifyJWTToken token = do
  secretKey <- secret <$> asks jwtSecret
  timeNow <- numericDate <$> liftIO getPOSIXTime
  case claims <$> decodeAndVerifySignature secretKey token of
    Just claimsSet ->
      case (,) <$> timeNow <*> Web.JWT.exp claimsSet of
        Just (now, expiryTimeInToken) ->
          if expiryTimeInToken < now then
            return Nothing
          else
            return . Just $ unregisteredClaims claimsSet
        Nothing -> return Nothing
    Nothing -> return Nothing
