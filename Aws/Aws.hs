{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}
module Aws.Aws
( -- * Logging
  LogLevel(..)
, Logger
, defaultLog
  -- * Configuration
, Configuration(..)
, baseConfiguration
, dbgConfiguration
  -- * Transaction runners
  -- ** Safe runners
, aws
, awsRef
, simpleAws
, simpleAwsRef
  -- ** Unsafe runners
, unsafeAws
, unsafeAwsRef
  -- ** URI runners
, awsUri
)
where

import           Aws.Core
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

-- | The severity of a log message, in rising order.
data LogLevel
    = Debug
    | Info
    | Warning
    | Error
    deriving (Show, Eq, Ord)

-- | The interface for any logging function. Takes log level and a log message, and can perform an arbitrary
-- IO action.
type Logger = LogLevel -> T.Text -> IO ()

-- | The default logger @defaultLog minLevel@, which prints log messages above level @minLevel@ to @stderr@.
defaultLog :: LogLevel -> Logger
defaultLog minLevel lev t | lev >= minLevel = T.hPutStrLn stderr $ T.concat [T.pack $ show lev, ": ", t]
                          | otherwise       = return ()

-- | The configuration for an AWS request. You can use multiple configurations in parallel, even over the same HTTP
-- connection manager.
data Configuration
    = Configuration {
        -- | Whether to restrict the signature validity with a plain timestamp, or with explicit expiration
        -- (absolute or relative).
        timeInfo :: TimeInfo
        -- | AWS access credentials.
      , credentials :: Credentials
        -- | The error / message logger.
      , logger :: Logger
      }

-- | The default configuration, with credentials loaded from environment variable or configuration file
-- (see 'loadCredentialsDefault').

baseConfiguration :: IO Configuration
baseConfiguration = do
  Just cr <- loadCredentialsDefault
  return Configuration {
                      timeInfo = Timestamp
                    , credentials = cr
                    , logger = defaultLog Warning
                    }
-- TODO: better error handling when credentials cannot be loaded

-- | Debug configuration, which avoids using HTTPS for some queries. DO NOT USE THIS IN PRODUCTION!
dbgConfiguration :: IO Configuration
dbgConfiguration = do
  c <- baseConfiguration
  return c { logger = defaultLog Debug }

-- | Run an AWS transaction, with HTTP manager and metadata wrapped in a 'Response'.
-- 
-- All errors are caught and wrapped in the 'Response' value.
-- 
-- Usage (with existing 'HTTP.Manager'):
-- @
--     resp <- aws cfg serviceCfg manager request
-- @
aws :: (Transaction r a)
      => Configuration -> ServiceConfiguration r -> HTTP.Manager -> r -> IO (Response (ResponseMetadata a) a)
aws = unsafeAws

-- | Run an AWS transaction, with HTTP manager and metadata returned in an 'IORef'.
-- 
-- Errors are not caught, and need to be handled with exception handlers.
-- 
-- Usage (with existing 'HTTP.Manager'):
-- @
--     ref <- newIORef mempty;
--     resp <- awsRef cfg serviceCfg manager request
-- @

-- Unfortunately, the ";" above seems necessary, as haddock does not want to split lines for me.
awsRef :: (Transaction r a)
      => Configuration -> ServiceConfiguration r -> HTTP.Manager -> IORef (ResponseMetadata a) -> r -> IO a
awsRef = unsafeAwsRef

-- | Run an AWS transaction, /without/ HTTP manager and with metadata wrapped in a 'Response'.
-- 
-- Note that this is potentially less efficient than using 'aws', because HTTP connections cannot be re-used.
-- 
-- All errors are caught and wrapped in the 'Response' value.
-- 
-- Usage:
-- @
--     resp <- simpleAws cfg serviceCfg request
-- @
simpleAws :: (Transaction r a)
            => Configuration -> ServiceConfiguration r -> r -> IO (Response (ResponseMetadata a) a)
simpleAws cfg scfg request = HTTP.withManager $ \manager -> liftIO $ aws cfg scfg manager request

-- | Run an AWS transaction, /without/ HTTP manager and with metadata returned in an 'IORef'.
-- 
-- Errors are not caught, and need to be handled with exception handlers.
-- 
-- Usage:
-- @
--     ref <- newIORef mempty;
--     resp <- simpleAwsRef cfg serviceCfg request
-- @

-- Unfortunately, the ";" above seems necessary, as haddock does not want to split lines for me.
simpleAwsRef :: (Transaction r a)
            => Configuration -> ServiceConfiguration r -> IORef (ResponseMetadata a) -> r -> IO a
simpleAwsRef cfg scfg metadataRef request = HTTP.withManager $ 
                                              \manager -> liftIO $ awsRef cfg scfg manager metadataRef request

-- | Run an AWS transaction, without enforcing that response and request type form a valid transaction pair.
-- 
-- This is especially useful for debugging and development, you should not have to use it in production.
-- 
-- All errors are caught and wrapped in the 'Response' value.
unsafeAws
  :: (ResponseConsumer r a,
      Monoid (ResponseMetadata a),
      SignQuery r) =>
     Configuration -> ServiceConfiguration r -> HTTP.Manager -> r -> IO (Response (ResponseMetadata a) a)
unsafeAws cfg scfg manager request = do
  metadataRef <- newIORef mempty
  resp <- attemptIO (id :: E.SomeException -> E.SomeException) $
            unsafeAwsRef cfg scfg manager metadataRef request
  metadata <- readIORef metadataRef
  return $ Response metadata resp

-- | Run an AWS transaction, without enforcing that response and request type form a valid transaction pair.
-- 
-- This is especially useful for debugging and development, you should not have to use it in production.
-- 
-- Errors are not caught, and need to be handled with exception handlers.
unsafeAwsRef
  :: (ResponseConsumer r a,
      Monoid (ResponseMetadata a),
      SignQuery r) =>
     Configuration -> ServiceConfiguration r -> HTTP.Manager -> IORef (ResponseMetadata a) -> r -> IO a
unsafeAwsRef cfg info manager metadataRef request = do
  sd <- signatureData <$> timeInfo <*> credentials $ cfg
  let q = signQuery request info sd
  logger cfg Debug $ T.pack $ "String to sign: " ++ show (sqStringToSign q)
  let httpRequest = queryToHttpRequest q
  logger cfg Debug $ T.pack $ "Host: " ++ show (HTTP.host httpRequest)
  resp <- runResourceT $ do
      HTTP.Response status _ headers body <- HTTP.http httpRequest manager
      responseConsumer request metadataRef status headers body
  return resp

-- | Run a URI-only AWS transaction. Returns a URI that can be sent anywhere. Does not work with all requests.
-- 
-- Usage:
-- @
--     uri <- awsUri cfg request
-- @
awsUri :: (SignQuery request)
         => Configuration -> ServiceConfiguration request -> request -> IO B.ByteString
awsUri cfg info request = do
  let ti = timeInfo cfg
      cr = credentials cfg
  sd <- signatureData ti cr
  let q = signQuery request info sd
  logger cfg Debug $ T.pack $ "String to sign: " ++ show (sqStringToSign q)
  return $ queryToUri q

