{-# LANGUAGE RecordWildCards #-}

module Aws.Credentials
where
  
import           Aws.Query
import           Aws.Util
import           Control.Applicative
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Shortcircuit     (orM)
import           Data.HMAC
import           Data.List
import           Data.Ord
import           Data.Time
import           System.Directory
import           System.Environment
import           System.FilePath
import qualified Codec.Binary.Base64      as Base64
import qualified Codec.Binary.UTF8.String as Utf8

data Credentials
    = Credentials {
        accessKeyID :: String
      , secretAccessKey :: String
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
    return Credentials { accessKeyID = keyID, secretAccessKey = secret }
      where
        hasKey _ [] = False
        hasKey k (k2 : _) = k == k2

loadCredentialsFromEnv :: MonadIO io => io (Maybe Credentials)
loadCredentialsFromEnv = liftIO $ do
  env <- getEnvironment
  let lk = flip lookup env
      keyID = lk "AWS_ACCESS_KEY_ID"
      secret = lk "AWS_ACCESS_KEY_SECRET" `mplus` lk "AWS_SECRET_ACCESS_KEY"
  return (Credentials <$> keyID <*> secret)
  
loadCredentialsFromEnvOrFile :: MonadIO io => FilePath -> String -> io (Maybe Credentials)
loadCredentialsFromEnvOrFile file key = loadCredentialsFromEnv `orM` loadCredentialsFromFile file key

loadCredentialsDefault :: MonadIO io => io (Maybe Credentials)
loadCredentialsDefault = do
  file <- credentialsDefaultFile
  loadCredentialsFromEnvOrFile file credentialsDefaultKey

makeAbsoluteTimeInfo :: MonadIO io => TimeInfo -> io AbsoluteTimeInfo
makeAbsoluteTimeInfo Timestamp     = AbsoluteTimestamp `liftM` liftIO getCurrentTime
makeAbsoluteTimeInfo (ExpiresAt t) = return $ AbsoluteExpires t
makeAbsoluteTimeInfo (ExpiresIn s) = (AbsoluteExpires . addUTCTime s) `liftM` liftIO getCurrentTime

addTimeInfo :: AbsoluteTimeInfo -> Query -> Query
addTimeInfo (AbsoluteTimestamp time) q@Query{..} 
    = q {
        query = ("Timestamp", fmtAmzTime time) : query
      , date = Just time
      }
addTimeInfo (AbsoluteExpires time) q@Query{..} 
    = q {
        query = ("Expires", fmtAmzTime time) : query
      }

addSignatureData :: Credentials -> Query -> Query
addSignatureData Credentials{..} q@Query{..} 
    = q {
        query = [("AWSAccessKeyId", accessKeyID), ("SignatureMethod", "HmacSHA1"), ("SignatureVersion", "2")]
                ++ query
      }

stringToSign :: Query -> String
stringToSign Query{..} 
    = case api of 
        SimpleDB -> show method ++ "\n" ++
                    host ++ "\n" ++
                    path ++ "\n" ++
                    urlEncodeVars sortedQuery
    where sortedQuery = sortBy (comparing $ Utf8.encode . fst) query
                                               
signPreparedQuery :: Credentials -> Query -> Query
signPreparedQuery Credentials{..} q@Query{..} = q { query = ("Signature", sig) : query }
    where sig = Base64.encode $ hmac_sha1 (Utf8.encode secretAccessKey) (Utf8.encode . stringToSign $ q)
                           
signQueryAbsolute :: AbsoluteTimeInfo -> Credentials -> Query -> Query
signQueryAbsolute ti cr = signPreparedQuery cr . addSignatureData cr . addTimeInfo ti

signQuery :: MonadIO io => TimeInfo -> Credentials -> Query -> io Query
signQuery ti cr q = do
  ti' <- makeAbsoluteTimeInfo ti
  return $ signQueryAbsolute ti' cr q
