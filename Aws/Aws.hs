{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, UndecidableInstances, FlexibleContexts #-}
module Aws.Aws
where

import           Aws.Credentials
import           Aws.Debug
import           Aws.Query
import           Aws.Response
import           Aws.S3.Info
import           Aws.Signature
import           Aws.SimpleDb.Info
import           Aws.Transaction
import           Control.Applicative
import           Control.Monad.Reader
import qualified Data.ByteString         as B
import qualified Data.Enumerator         as En
import qualified Network.HTTP.Enumerator as HTTP

data Configuration
    = Configuration {
       timeInfo :: TimeInfo
      , credentials :: Credentials
      , sdbInfo :: SdbInfo
      , sdbInfoUri :: SdbInfo
      , s3Info :: S3Info
      , s3InfoUri :: S3Info
      }

class ConfigurationFetch a where
    configurationFetch :: Configuration -> a
    configurationFetchUri :: Configuration -> a
    configurationFetchUri = configurationFetch

instance ConfigurationFetch () where
    configurationFetch _ = ()

instance ConfigurationFetch SdbInfo where
    configurationFetch = sdbInfo
    configurationFetchUri = sdbInfoUri

instance ConfigurationFetch S3Info where
    configurationFetch = s3Info
    configurationFetchUri = s3InfoUri

baseConfiguration :: MonadIO io => io Configuration
baseConfiguration = do
  Just cr <- loadCredentialsDefault
  return Configuration {
                      timeInfo = Timestamp
                    , credentials = cr
                    , sdbInfo = sdbHttpsPost sdbUsEast
                    , sdbInfoUri = sdbHttpsGet sdbUsEast
                    , s3Info = s3Http
                    , s3InfoUri = s3Http
                    }
-- TODO: better error handling when credentials cannot be loaded

debugConfiguration :: MonadIO io => io Configuration
debugConfiguration = do 
  c <- baseConfiguration
  return c { sdbInfo = sdbHttpPost sdbUsEast, sdbInfoUri = sdbHttpGet sdbUsEast  }

newtype AwsT m a = AwsT { fromAwsT :: ReaderT Configuration m a }

type Aws = AwsT IO

runAws :: AwsT m a -> Configuration -> m a
runAws = runReaderT . fromAwsT

runAws' :: MonadIO io => AwsT io a -> io a
runAws' a = baseConfiguration >>= runAws a

runAwsDebug :: MonadIO io => AwsT io a -> io a
runAwsDebug a = debugConfiguration >>= runAws a

instance Monad m => Monad (AwsT m) where
    return = AwsT . return
    m >>= k = AwsT $ fromAwsT m >>= fromAwsT . k

instance MonadIO m => MonadIO (AwsT m) where
    liftIO = AwsT . liftIO

class MonadIO aws => MonadAws aws where
    configuration :: MonadAws aws => aws Configuration

instance MonadIO m => MonadAws (AwsT m) where
    configuration = AwsT ask

aws :: (Transaction request response
       , ConfigurationFetch (Info request)
       , MonadAws aws) 
      => request -> aws response
aws request = do
  cfg <- configuration
  sd <- liftIO $ signatureData <$> timeInfo <*> credentials $ cfg
  let info = configurationFetch cfg
  let q = signQuery request info sd
  debugPrint "String to sign" $ sqStringToSign q
  let httpRequest = queryToHttpRequest q
  liftIO $ En.run_ $ HTTP.httpRedirect httpRequest responseIteratee

awsUri :: (SignQuery request
          , ConfigurationFetch (Info request)
          , MonadAws aws)
         => request -> aws B.ByteString
awsUri request = do
  cfg <- configuration
  let ti = timeInfo cfg
      cr = credentials cfg
      info = configurationFetchUri cfg
  sd <- liftIO $ signatureData ti cr
  let q = signQuery request info sd
  debugPrint "String to sign" $ sqStringToSign q
  return $ queryToUri q
