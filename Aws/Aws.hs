{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, UndecidableInstances, FlexibleContexts #-}
module Aws.Aws
where

import Aws.Credentials
import Aws.Http
import Aws.SimpleDb.Error
import Aws.SimpleDb.Info
import Aws.Transaction
import Aws.Util
import Control.Monad.Reader
import Network.Curl.Opts

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
  return $ Configuration {
               http = http'
             , timeInfo = Timestamp
             , credentials = cr
             , sdbInfo = sdbHttpsPost sdbUsEast
             }
-- TODO: better error handling when credentials cannot be loaded

curlConfiguration :: [CurlOption] -> IO Configuration
curlConfiguration curlOpt = baseConfiguration (curlRequest curlOpt)

newtype Aws a = MkAws { fromAws :: ReaderT Configuration IO a }

runAws :: Aws a -> Configuration -> IO a
runAws = runReaderT . fromAws

instance Monad Aws where
    return = MkAws . return
    m >>= k = MkAws $ fromAws m >>= fromAws . k

instance MonadIO Aws where
    liftIO = MkAws . liftIO

configuration :: Aws Configuration
configuration = MkAws ask

aws :: (Transaction request info response error, ConfigurationFetch info) => request -> Aws (Either error response)
aws request = do
  cfg <- configuration
  liftM4 transact http timeInfo credentials configurationFetch cfg request
