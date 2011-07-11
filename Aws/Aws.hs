{-# LANGUAGE FlexibleContexts #-}
module Aws.Aws
where

import           Aws.Credentials
import           Aws.Debug
import           Aws.Http
import           Aws.Query
import           Aws.Response
import           Aws.S3.Info
import           Aws.Signature
import           Aws.SimpleDb.Info
import           Aws.Sqs.Info
import           Aws.Transaction
import           Control.Applicative
import           Data.Attempt            (attemptIO)
import           Data.IORef
import           Data.Monoid
import qualified Control.Exception       as E
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
      , sqsInfo :: SqsInfo
      , sqsInfoUri :: SqsInfo
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

instance ConfigurationFetch SqsInfo where
    configurationFetch = sqsInfo
    configurationFetchUri = sqsInfoUri

baseConfiguration :: IO Configuration
baseConfiguration = do
  Just cr <- loadCredentialsDefault
  return Configuration {
                      timeInfo = Timestamp
                    , credentials = cr
                    , sdbInfo = sdbHttpsPost sdbUsEast
                    , sdbInfoUri = sdbHttpsGet sdbUsEast
                    , s3Info = s3 HTTP s3EndpointUsClassic False
                    , s3InfoUri = s3 HTTP s3EndpointUsClassic True
                    , sqsInfo = sqs HTTP sqsEndpointUsClassic False
                    , sqsInfoUri = sqs HTTP sqsEndpointUsClassic True
                    }
-- TODO: better error handling when credentials cannot be loaded

debugConfiguration :: IO Configuration
debugConfiguration = do 
  c <- baseConfiguration
  return c { sdbInfo = sdbHttpPost sdbUsEast, sdbInfoUri = sdbHttpGet sdbUsEast  }

aws :: (Transaction r a
       , ConfigurationFetch (Info r)) 
      => Configuration -> r -> IO (Response (ResponseMetadata a) a)
aws = unsafeAws

unsafeAws
  :: (ResponseIteratee r a,
      Monoid (ResponseMetadata a),
      SignQuery r,
      ConfigurationFetch (Info r)) =>
     Configuration -> r -> IO (Response (ResponseMetadata a) a)
unsafeAws cfg request = do
  sd <- signatureData <$> timeInfo <*> credentials $ cfg
  let info = configurationFetch cfg
  let q = signQuery request info sd
  debugPrint "String to sign" $ sqStringToSign q
  let httpRequest = queryToHttpRequest q
  metadataRef <- newIORef mempty
  resp <- attemptIO (id :: E.SomeException -> E.SomeException) $
          HTTP.withManager $ En.run_ . HTTP.httpRedirect httpRequest (responseIteratee request metadataRef)
  metadata <- readIORef metadataRef
  return $ Response metadata resp

awsUri :: (SignQuery request
          , ConfigurationFetch (Info request))
         => Configuration -> request -> IO B.ByteString
awsUri cfg request = do
  let ti = timeInfo cfg
      cr = credentials cfg
      info = configurationFetchUri cfg
  sd <- signatureData ti cr
  let q = signQuery request info sd
  debugPrint "String to sign" $ sqStringToSign q
  return $ queryToUri q

