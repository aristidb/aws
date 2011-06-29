module Aws.Credentials
where
  
import           Control.Applicative
import           Control.Monad
import           Control.Shortcircuit      (orM)
import           Data.List
import           System.Directory
import           System.Environment
import           System.FilePath
import qualified Data.ByteString           as B
import qualified Data.ByteString.UTF8      as BU

data Credentials
    = Credentials {
        accessKeyID :: B.ByteString
      , secretAccessKey :: B.ByteString
      }
    deriving (Show)
             
credentialsDefaultFile :: IO FilePath
credentialsDefaultFile = (</> ".aws-keys") <$> getHomeDirectory

credentialsDefaultKey :: String
credentialsDefaultKey = "default"

loadCredentialsFromFile :: FilePath ->  String -> IO (Maybe Credentials)
loadCredentialsFromFile file key = do
  contents <- map words . lines <$> readFile file
  return $ do 
    [_key, keyID, secret] <- find (hasKey key) contents
    return Credentials { accessKeyID = BU.fromString keyID, secretAccessKey = BU.fromString secret }
      where
        hasKey _ [] = False
        hasKey k (k2 : _) = k == k2

loadCredentialsFromEnv :: IO (Maybe Credentials)
loadCredentialsFromEnv = do
  env <- getEnvironment
  let lk = flip lookup env
      keyID = lk "AWS_ACCESS_KEY_ID"
      secret = lk "AWS_ACCESS_KEY_SECRET" `mplus` lk "AWS_SECRET_ACCESS_KEY"
  return (Credentials <$> (BU.fromString <$> keyID) <*> (BU.fromString <$> secret))
  
loadCredentialsFromEnvOrFile :: FilePath -> String -> IO (Maybe Credentials)
loadCredentialsFromEnvOrFile file key = loadCredentialsFromEnv `orM` loadCredentialsFromFile file key

loadCredentialsDefault :: IO (Maybe Credentials)
loadCredentialsDefault = do
  file <- credentialsDefaultFile
  loadCredentialsFromEnvOrFile file credentialsDefaultKey
