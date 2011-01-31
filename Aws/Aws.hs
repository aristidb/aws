{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, UndecidableInstances, FlexibleContexts #-}
module Aws.Aws
where

import           Aws.Credentials
import           Aws.Http
import           Aws.Query
import           Aws.SimpleDb.Info
import           Aws.Transaction
import           Control.Monad.Reader
import qualified Data.ByteString       as B

data Configuration
    = Configuration {
       timeInfo :: TimeInfo
      , credentials :: Credentials
      , sdbInfo :: SdbInfo
      }

class ConfigurationFetch a where
    configurationFetch :: Configuration -> a

instance ConfigurationFetch SdbInfo where
    configurationFetch = sdbInfo

baseConfiguration :: MonadIO io => io Configuration
baseConfiguration = do
  Just cr <- loadCredentialsDefault
  return Configuration {
               timeInfo = Timestamp
             , credentials = cr
             , sdbInfo = sdbHttpsGet sdbUsEast -- POST does not work properly, https://github.com/aristidb/aws/issues/issue/1
             }
-- TODO: better error handling when credentials cannot be loaded

debugConfiguration :: MonadIO io => io Configuration
debugConfiguration = liftM (\c -> c { sdbInfo = sdbHttpPost sdbUsEast }) baseConfiguration

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
  liftIO $ liftM3 transact timeInfo credentials configurationFetch cfg request

awsUri :: (AsQuery request
          , ConfigurationFetch (Info request)
          , MonadAws aws)
         => request -> aws B.ByteString
awsUri request = do
  cfg <- configuration
  let ti = timeInfo cfg
      cr = credentials cfg
      info = configurationFetch cfg
  let uq = asQuery info request
      uq' = uq { method = Get }
  q <- signQuery ti cr uq'
  return $ queryToUri q
