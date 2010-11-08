{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, UndecidableInstances, FlexibleContexts #-}
module Aws.Aws
where

import           Aws.Credentials
import           Aws.Http
import           Aws.SimpleDb.Info
import           Aws.Transaction
import           Control.Monad.Reader
import           Network.Curl.Opts
import qualified Control.Monad.CatchIO as C

data Configuration
    = Configuration {
        http :: HttpRequest -> IO HttpResponse
      , timeInfo :: TimeInfo
      , credentials :: Credentials
      , sdbInfo :: SdbInfo
      }

class ConfigurationFetch a where
    configurationFetch :: Configuration -> a

instance ConfigurationFetch SdbInfo where
    configurationFetch = sdbInfo

baseConfiguration :: (HttpRequest -> IO HttpResponse) -> IO Configuration
baseConfiguration http' = do
  Just cr <- loadCredentialsDefault
  return Configuration {
               http = http'
             , timeInfo = Timestamp
             , credentials = cr
             , sdbInfo = sdbHttpsPost sdbUsEast
             }
-- TODO: better error handling when credentials cannot be loaded

curlConfiguration :: [CurlOption] -> IO Configuration
curlConfiguration curlOpt = baseConfiguration (curlRequest curlOpt)

newtype AwsT m a = AwsT { fromAwsT :: ReaderT Configuration m a }

type Aws = AwsT IO

runAwsT :: AwsT m a -> Configuration -> m a
runAwsT = runReaderT . fromAwsT

runAws :: Aws a -> Configuration -> IO a
runAws = runAwsT

instance Monad m => Monad (AwsT m) where
    return = AwsT . return
    m >>= k = AwsT $ fromAwsT m >>= fromAwsT . k

instance MonadIO m => MonadIO (AwsT m) where
    liftIO = AwsT . liftIO

class MonadIO aws => MonadAws aws where
    configuration :: MonadAws aws => aws Configuration

instance MonadIO m => MonadAws (AwsT m) where
    configuration = AwsT ask

instance C.MonadCatchIO m => C.MonadCatchIO (AwsT m) where
    m `catch` f = AwsT $ fromAwsT m `C.catch` (fromAwsT . f)
    block = AwsT . C.block . fromAwsT
    unblock = AwsT . C.unblock . fromAwsT

aws :: (Transaction request info response error, ConfigurationFetch info, MonadAws aws) 
       => request -> aws response
aws request = do
  cfg <- configuration
  liftM4 transact http timeInfo credentials configurationFetch cfg request
