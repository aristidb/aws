{-# LANGUAGE RecordWildCards, OverloadedStrings #-}

module Aws.Credentials
where
  
import           Aws.Http
import           Aws.Query
import           Aws.Util
import           Control.Applicative
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.State
import           Control.Shortcircuit   (orM)
import           Data.List
import           Data.Maybe
import           Data.Ord
import           Data.Time
import           System.Directory
import           System.Environment
import           System.FilePath
import qualified Crypto.HMAC            as HMAC
import qualified Crypto.Hash.SHA256     as SHA256
import qualified Data.ByteString        as B
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.UTF8   as BU
import qualified Data.Serialize         as Serialize

data Credentials
    = Credentials {
        accessKeyID :: B.ByteString
      , secretAccessKey :: B.ByteString
      }
    deriving (Show)

data AbsoluteTimeInfo
    = AbsoluteTimestamp { fromAbsoluteTimestamp :: UTCTime }
    | AbsoluteExpires { fromAbsoluteExpires :: UTCTime }
    deriving (Show)

data TimeInfo
    = Timestamp
    | ExpiresAt { fromExpiresAt :: UTCTime }
    | ExpiresIn { fromExpiresIn :: NominalDiffTime }
    deriving (Show)
             
credentialsDefaultFile :: MonadIO io => io FilePath
credentialsDefaultFile = liftIO $ (</> ".aws-keys") <$> getHomeDirectory

credentialsDefaultKey :: String
credentialsDefaultKey = "default"

loadCredentialsFromFile :: MonadIO io => FilePath ->  String -> io (Maybe Credentials)
loadCredentialsFromFile file key = liftIO $ do
  contents <- map words . lines <$> readFile file
  return $ do 
    [_key, keyID, secret] <- find (hasKey key) contents
    return Credentials { accessKeyID = BU.fromString keyID, secretAccessKey = BU.fromString secret }
      where
        hasKey _ [] = False
        hasKey k (k2 : _) = k == k2

loadCredentialsFromEnv :: MonadIO io => io (Maybe Credentials)
loadCredentialsFromEnv = liftIO $ do
  env <- getEnvironment
  let lk = flip lookup env
      keyID = lk "AWS_ACCESS_KEY_ID"
      secret = lk "AWS_ACCESS_KEY_SECRET" `mplus` lk "AWS_SECRET_ACCESS_KEY"
  return (Credentials <$> (BU.fromString <$> keyID) <*> (BU.fromString <$> secret))
  
loadCredentialsFromEnvOrFile :: MonadIO io => FilePath -> String -> io (Maybe Credentials)
loadCredentialsFromEnvOrFile file key = loadCredentialsFromEnv `orM` loadCredentialsFromFile file key

loadCredentialsDefault :: MonadIO io => io (Maybe Credentials)
loadCredentialsDefault = do
  file <- credentialsDefaultFile
  loadCredentialsFromEnvOrFile file credentialsDefaultKey

makeAbsoluteTimeInfo :: TimeInfo -> UTCTime -> AbsoluteTimeInfo
makeAbsoluteTimeInfo Timestamp     now = AbsoluteTimestamp now
makeAbsoluteTimeInfo (ExpiresAt t) _   = AbsoluteExpires t
makeAbsoluteTimeInfo (ExpiresIn s) now = AbsoluteExpires $ addUTCTime s now

stringToSign :: Query -> B.ByteString
stringToSign Query{..} 
    = case api of 
        SimpleDB -> B.intercalate "\n" [httpMethod method
                                       , host
                                       , path
                                       , urlEncodeVarsBS False sortedQuery]
        S3 -> B.intercalate "\n" [httpMethod method
                                 , fromMaybe "" contentMd5
                                 , fromMaybe "" contentType
                                 , fromMaybe "" $ fmtAmzTime `fmap` date
                                 , "" -- canonicalized AMZ headers
                                 , canonicalizedResource]
    where sortedQuery = sortBy (comparing fst) query

signature :: Credentials -> Query -> B.ByteString
signature cr q = Base64.encode $ Serialize.encode (HMAC.hmac' key input :: SHA256.SHA256)
    where key = HMAC.MacKey (secretAccessKey cr)
          input = stringToSign q

signQuery :: MonadIO io => TimeInfo -> Credentials -> Query -> io Query
signQuery rti cr query = flip execStateT query $ do
  now <- liftIO getCurrentTime
  let ti = makeAbsoluteTimeInfo rti now
  modify $ \q -> q { date = Just now }
  am <- authorizationMethod `liftM` get
  case am of
    AuthorizationNone -> return ()
    AuthorizationQuery -> do
            modify $ addQuery $ case ti of
                                  (AbsoluteTimestamp time) -> [("Timestamp", fmtAmzTime time)]
                                  (AbsoluteExpires time) -> [("Expires", fmtAmzTime time)]
            modify $ addQuery [("AWSAccessKeyId", accessKeyID cr), ("SignatureMethod", "HmacSHA256"), ("SignatureVersion", "2")]
            modify $ \q -> addQuery [("Signature", signature cr q)] q

