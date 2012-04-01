{-# LANGUAGE OverloadedStrings #-}
module Aws.Credentials
where
  
import           Control.Applicative
import           Control.Monad
import           Data.List
import           System.Directory
import           System.Environment
import           System.FilePath
import qualified Data.ByteString      as B
import qualified Data.Text            as T
import qualified Data.Text.Encoding   as T
import qualified Data.Text.IO         as T

data Credentials
    = Credentials {
        accessKeyID :: B.ByteString
      , secretAccessKey :: B.ByteString
      }
    deriving (Show)
             
credentialsDefaultFile :: IO FilePath
credentialsDefaultFile = (</> ".aws-keys") <$> getHomeDirectory

credentialsDefaultKey :: T.Text
credentialsDefaultKey = "default"

loadCredentialsFromFile :: FilePath -> T.Text -> IO (Maybe Credentials)
loadCredentialsFromFile file key = do
  contents <- map T.words . T.lines <$> T.readFile file
  return $ do 
    [_key, keyID, secret] <- find (hasKey key) contents
    return Credentials { accessKeyID = T.encodeUtf8 keyID, secretAccessKey = T.encodeUtf8 secret }
      where
        hasKey _ [] = False
        hasKey k (k2 : _) = k == k2

loadCredentialsFromEnv :: IO (Maybe Credentials)
loadCredentialsFromEnv = do
  env <- getEnvironment
  let lk = flip lookup env
      keyID = lk "AWS_ACCESS_KEY_ID"
      secret = lk "AWS_ACCESS_KEY_SECRET" `mplus` lk "AWS_SECRET_ACCESS_KEY"
  return (Credentials <$> (T.encodeUtf8 . T.pack <$> keyID) <*> (T.encodeUtf8 . T.pack <$> secret))
  
loadCredentialsFromEnvOrFile :: FilePath -> T.Text -> IO (Maybe Credentials)
loadCredentialsFromEnvOrFile file key = 
  do
    envcr <- loadCredentialsFromEnv
    case envcr of
      Just cr -> return (Just cr)
      Nothing -> loadCredentialsFromFile file key

loadCredentialsDefault :: IO (Maybe Credentials)
loadCredentialsDefault = do
  file <- credentialsDefaultFile
  loadCredentialsFromEnvOrFile file credentialsDefaultKey
