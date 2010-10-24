{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, UndecidableInstances, FlexibleContexts #-}
module AWS.Aws
where

import AWS.Credentials
import AWS.Http
import AWS.Transaction
import AWS.SimpleDb.Info
import AWS.SimpleDb.Error
import Control.Applicative
import MonadLib
import MonadLib.Derive

data Configuration
    = Configuration {
        http :: (HttpRequest -> IO HttpResponse)
      , timeInfo :: TimeInfo
      , credentials :: Credentials
      , sdbInfo :: SdbInfo
      }

baseConfiguration http = do
  Just cr <- loadCredentialsDefault
  return $ Configuration {
               http = http
             , timeInfo = Timestamp
             , credentials = cr
             , sdbInfo = sdbHttpsPost sdbUsEast
             }
-- TODO: better error handling when credentials cannot be loaded

curlConfiguration curlOpt = baseConfiguration (curlRequest curlOpt)

newtype Aws a = MkAws { fromAws :: ReaderT Configuration IO a }

isoAws = Iso MkAws fromAws

runAws c = runReaderT c . fromAws

instance Monad Aws where
    return = derive_return isoAws
    (>>=) = derive_bind isoAws

instance RunM Aws a (Configuration -> IO a) where
    runM = derive_runM isoAws

instance BaseM Aws IO where
    inBase = derive_inBase isoAws

configuration :: Aws Configuration
configuration = MkAws ask

aws :: (Transaction request info response error) => info -> request -> Aws (Either error response)
aws info request = do
  cfg <- configuration
  inBase $ transact (http cfg) (timeInfo cfg) (credentials cfg) info request

sdb :: (Transaction request SdbInfo response SdbError) => request -> Aws (Either SdbError response)
sdb request = do
  cfg <- configuration
  aws (sdbInfo cfg) request
