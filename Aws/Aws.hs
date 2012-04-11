{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}
module Aws.Aws
where

import           Aws.Credentials
import           Aws.Http
import           Aws.Query
import           Aws.Response
import           Aws.S3.Info
import           Aws.Ses.Info
import           Aws.Signature
import           Aws.SimpleDb.Info
import           Aws.Sqs.Info
import           Aws.Transaction
import           Control.Applicative
import           Control.Monad.Trans  (liftIO)
import           Data.Attempt         (attemptIO)
import           Data.Conduit         (runResourceT)
import           Data.IORef
import           Data.Monoid
import           System.IO            (stderr)
import qualified Control.Exception    as E
import qualified Data.ByteString      as B
import qualified Data.Text            as T
import qualified Data.Text.IO         as T
import qualified Network.HTTP.Conduit as HTTP

data LogLevel
    = Debug
    | Info
    | Warning
    | Error
    deriving (Show, Eq, Ord)

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
      , sesInfo :: SesInfo
      , sesInfoUri :: SesInfo
      , logger :: LogLevel -> T.Text -> IO ()
      }

defaultLog :: LogLevel -> LogLevel -> T.Text -> IO ()
defaultLog minLevel lev t | lev >= minLevel = T.hPutStrLn stderr $ T.concat [T.pack $ show lev, ": ", t]
                          | otherwise       = return ()

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

instance ConfigurationFetch SesInfo where
    configurationFetch = sesInfo
    configurationFetchUri = sesInfoUri

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
                    , sesInfo = sesHttpsPost sesUsEast
                    , sesInfoUri = sesHttpsGet sesUsEast
                    , logger = defaultLog Warning
                    }
-- TODO: better error handling when credentials cannot be loaded

debugConfiguration :: IO Configuration
debugConfiguration = do
  c <- baseConfiguration
  return c {
      sdbInfo = sdbHttpPost sdbUsEast
    , sdbInfoUri = sdbHttpGet sdbUsEast
    , logger = defaultLog Debug
    }

aws :: (Transaction r a
       , ConfigurationFetch (Info r))
      => Configuration -> HTTP.Manager -> r -> IO (Response (ResponseMetadata a) a)
aws = unsafeAws

awsRef :: (Transaction r a
       , ConfigurationFetch (Info r))
      => Configuration -> HTTP.Manager -> IORef (ResponseMetadata a) -> r -> IO a
awsRef = unsafeAwsRef

simpleAws :: (Transaction r a
             , ConfigurationFetch (Info r))
            => Configuration -> r -> IO (Response (ResponseMetadata a) a)
simpleAws cfg request = HTTP.withManager $ \manager -> liftIO $ aws cfg manager request

simpleAwsRef :: (Transaction r a
             , ConfigurationFetch (Info r))
            => Configuration -> IORef (ResponseMetadata a) -> r -> IO a
simpleAwsRef cfg metadataRef request = HTTP.withManager $ \manager -> liftIO $ awsRef cfg manager metadataRef request

unsafeAws
  :: (ResponseConsumer r a,
      Monoid (ResponseMetadata a),
      SignQuery r,
      ConfigurationFetch (Info r)) =>
     Configuration -> HTTP.Manager -> r -> IO (Response (ResponseMetadata a) a)
unsafeAws cfg manager request = do
  metadataRef <- newIORef mempty
  resp <- attemptIO (id :: E.SomeException -> E.SomeException) $
            unsafeAwsRef cfg manager metadataRef request
  metadata <- readIORef metadataRef
  return $ Response metadata resp

unsafeAwsRef
  :: (ResponseConsumer r a,
      Monoid (ResponseMetadata a),
      SignQuery r,
      ConfigurationFetch (Info r)) =>
     Configuration -> HTTP.Manager -> IORef (ResponseMetadata a) -> r -> IO a
unsafeAwsRef cfg manager metadataRef request = do
  sd <- signatureData <$> timeInfo <*> credentials $ cfg
  let info = configurationFetch cfg
  let q = signQuery request info sd
  logger cfg Debug $ T.pack $ "String to sign: " ++ show (sqStringToSign q)
  let httpRequest = queryToHttpRequest q
  resp <- runResourceT $ do
      HTTP.Response status _ headers body <- HTTP.http httpRequest manager
      responseConsumer request metadataRef status headers body
  return resp

awsUri :: (SignQuery request
          , ConfigurationFetch (Info request))
         => Configuration -> request -> IO B.ByteString
awsUri cfg request = do
  let ti = timeInfo cfg
      cr = credentials cfg
      info = configurationFetchUri cfg
  sd <- signatureData ti cr
  let q = signQuery request info sd
  logger cfg Debug $ T.pack $ "String to sign: " ++ show (sqStringToSign q)
  return $ queryToUri q

