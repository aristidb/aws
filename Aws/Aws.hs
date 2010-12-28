{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, UndecidableInstances, FlexibleContexts #-}
module Aws.Aws
where

import           Aws.Credentials
import           Aws.Http
import           Aws.Query
import           Aws.SimpleDb.Info
import           Aws.Transaction
import           Aws.Response
import           Control.Monad.Reader
import           Network.URI             (URI)
import qualified Control.Failure         as F
import qualified Control.Monad.CatchIO   as C
import qualified Network.HTTP.Enumerator as HTTP

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

baseConfiguration :: IO Configuration
baseConfiguration = do
  Just cr <- loadCredentialsDefault
  return Configuration {
               timeInfo = Timestamp
             , credentials = cr
             , sdbInfo = sdbHttpsPost sdbUsEast
             }
-- TODO: better error handling when credentials cannot be loaded

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

instance (C.Exception e, MonadIO m) => F.Failure e (AwsT m) where
    failure = C.throw

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
         => request -> aws URI
awsUri request = do
  cfg <- configuration
  let ti = timeInfo cfg
      cr = credentials cfg
      info = configurationFetch cfg
  let uq = asQuery info request
      uq' = uq { method = Get }
  q <- signQuery ti cr uq'
  return $ queryToUri q
