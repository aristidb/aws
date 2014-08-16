{-# LANGUAGE CPP #-}
module Aws.Aws
( -- * Logging
  LogLevel(..)
, Logger
, defaultLog
  -- * Configuration
, Configuration(..)
, baseConfiguration
, dbgConfiguration
, Environment(..)
, closeEnvironment
, newDefaultEnvironment
, newDebugEnvironment
, withDefaultEnvironment
, withDebugEnvironment
  -- * Transaction runners
  -- ** Safe runners
, aws
, awsRef
, pureAws
, simpleAws
  -- ** Unsafe runners
, unsafeAws
, unsafeAwsRef
  -- ** URI runners
, awsUri
  -- * Iterated runners
--, awsIteratedAll
, awsIteratedSource
, awsIteratedList
)
where

import           Aws.Core
import           Control.Applicative
import qualified Control.Exception.Lifted as E
import           Control.Monad
import           Control.Monad.Base
import           Control.Monad.Trans
import           Control.Monad.Trans.Control
import           Control.Monad.Trans.Resource
import qualified Data.ByteString      as B
import qualified Data.CaseInsensitive as CI
import qualified Data.Conduit         as C
import qualified Data.Conduit.List    as CL
import           Data.IORef
import           Data.Monoid
import qualified Data.Text            as T
import qualified Data.Text.Encoding   as T
import qualified Data.Text.IO         as T
import qualified Network.HTTP.Conduit as HTTP
import           System.IO            (stderr)

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
baseConfiguration :: MonadBase IO io => io Configuration
baseConfiguration = liftBase $ do
  cr <- loadCredentialsDefault
  case cr of
    Nothing -> E.throw $ NoCredentialsException "could not locate aws credentials"
    Just cr' -> return Configuration {
                      timeInfo = Timestamp
                    , credentials = cr'
                    , logger = defaultLog Warning
                    }

-- | Debug configuration, which logs much more verbosely.
dbgConfiguration :: MonadBase IO io => io Configuration
dbgConfiguration = do
  c <- baseConfiguration
  return c { logger = defaultLog Debug }

data Environment = Environment {
        environmentConfiguration :: Configuration,
        environmentServiceConfigurationMap :: ServiceConfigurationMap,
        environmentDefaultServiceConfiguration :: (forall config. DefaultServiceConfiguration config => config),
        environmentHTTPManager :: HTTP.Manager
    }

environmentServiceConfiguration :: DefaultServiceConfiguration config => Environment -> config
environmentServiceConfiguration (Environment _ m d _) = getServiceConfiguration d m

closeEnvironment :: MonadBase IO io => Environment -> io ()
closeEnvironment = liftBase . HTTP.closeManager . environmentHTTPManager

newDefaultEnvironment :: MonadBase IO m => m Environment
newDefaultEnvironment = do
    cfg <- baseConfiguration
    mgr <- liftBase $ HTTP.newManager HTTP.conduitManagerSettings
    return (Environment cfg mempty defServiceConfig mgr)

newDebugEnvironment :: MonadBase IO m => m Environment
newDebugEnvironment = do
    cfg <- dbgConfiguration
    mgr <- liftBase $ HTTP.newManager HTTP.conduitManagerSettings
    return (Environment cfg mempty debugServiceConfig mgr)

withDefaultEnvironment :: MonadBaseControl IO m => (Environment -> m a) -> m a
withDefaultEnvironment = E.bracket newDefaultEnvironment closeEnvironment

withDebugEnvironment :: (Environment -> IO a) -> IO a
withDebugEnvironment = E.bracket newDebugEnvironment closeEnvironment

-- | Run an AWS transaction, with HTTP manager and metadata wrapped in a 'Response'.
-- 
-- All errors are caught and wrapped in the 'Response' value.
-- 
-- Metadata is logged at level 'Info'.
-- 
-- Usage (with existing 'Environment'):
-- @
--     resp <- aws env request
-- @
aws :: (Transaction r a, DefaultServiceConfiguration (ServiceConfiguration r NormalQuery))
     => Environment -> r -> ResourceT IO (Response (ResponseMetadata a) a)
aws env req = unsafeAws (environmentConfiguration env) (environmentServiceConfiguration env) (environmentHTTPManager env) req

-- | Run an AWS transaction, with HTTP manager and metadata returned in an 'IORef'.
-- 
-- Errors are not caught, and need to be handled with exception handlers.
-- 
-- Metadata is not logged.
-- 
-- Usage (with existing 'Environment'):
-- @
--     ref <- newIORef mempty;
--     resp <- awsRef env request
-- @

-- Unfortunately, the ";" above seems necessary, as haddock does not want to split lines for me.
awsRef :: (Transaction r a, DefaultServiceConfiguration (ServiceConfiguration r NormalQuery))
      => Environment
      -> IORef (ResponseMetadata a) 
      -> r 
      -> ResourceT IO a
awsRef env ref req = unsafeAwsRef (environmentConfiguration env) (environmentServiceConfiguration env) (environmentHTTPManager env) ref req

-- | Run an AWS transaction, with HTTP manager and without metadata.
-- 
-- Metadata is logged at level 'Info'.
-- 
-- Usage (with existing 'Environment'):
-- @
--     resp <- aws cfg serviceCfg manager request
-- @
pureAws :: (Transaction r a, DefaultServiceConfiguration (ServiceConfiguration r NormalQuery))
      => Environment
      -> r
      -> ResourceT IO a
pureAws env req = readResponseIO =<< aws env req

-- | Run an AWS transaction, /without/ HTTP manager and without metadata.
-- 
-- Metadata is logged at level 'Info'.
-- 
-- The whole response will be copied into memory, which is possibly not desirable for streaming responses.
-- 
-- Usage:
-- @
--     resp <- simpleAws env request
-- @
simpleAws :: (Transaction r a, DefaultServiceConfiguration (ServiceConfiguration r NormalQuery), AsMemoryResponse a, MonadBase IO io)
            => Environment
            -> r
            -> io (MemoryResponse a)
simpleAws env request
  = liftBase $ runResourceT $ loadToMemory =<< readResponseIO =<< aws env request

-- | Run an AWS transaction, without enforcing that response and request type form a valid transaction pair.
-- 
-- This is especially useful for debugging and development, you should not have to use it in production.
-- 
-- All errors are caught and wrapped in the 'Response' value.
-- 
-- Metadata is wrapped in the Response, and also logged at level 'Info'.
unsafeAws
  :: (ResponseConsumer r a,
      Monoid (ResponseMetadata a),
      Loggable (ResponseMetadata a),
      SignQuery r) =>
     Configuration -> ServiceConfiguration r NormalQuery -> HTTP.Manager -> r -> ResourceT IO (Response (ResponseMetadata a) a)
unsafeAws cfg scfg manager request = do
  metadataRef <- liftIO $ newIORef mempty

  let catchAll :: ResourceT IO a -> ResourceT IO (Either E.SomeException a)
      catchAll = E.handle (return . Left) . fmap Right

  resp <- catchAll $
            unsafeAwsRef cfg scfg manager metadataRef request
  metadata <- liftIO $ readIORef metadataRef
  liftIO $ logger cfg Info $ "Response metadata: " `mappend` toLogText metadata
  return $ Response metadata resp

-- | Run an AWS transaction, without enforcing that response and request type form a valid transaction pair.
-- 
-- This is especially useful for debugging and development, you should not have to use it in production.
-- 
-- Errors are not caught, and need to be handled with exception handlers.
-- 
-- Metadata is put in the 'IORef', but not logged.
unsafeAwsRef
  :: (ResponseConsumer r a,
      Monoid (ResponseMetadata a),
      SignQuery r) =>
     Configuration -> ServiceConfiguration r NormalQuery -> HTTP.Manager -> IORef (ResponseMetadata a) -> r -> ResourceT IO a
unsafeAwsRef cfg info manager metadataRef request = do
  sd <- liftIO $ signatureData <$> timeInfo <*> credentials $ cfg
  let q = signQuery request info sd
  liftIO $ logger cfg Debug $ T.pack $ "String to sign: " ++ show (sqStringToSign q)
  httpRequest <- liftIO $ queryToHttpRequest q
  liftIO $ logger cfg Debug $ T.pack $ "Host: " ++ show (HTTP.host httpRequest)
  hresp <- HTTP.http httpRequest manager
  forM_ (HTTP.responseHeaders hresp) $ \(hname,hvalue) -> liftIO $
    logger cfg Debug $ T.decodeUtf8 $ "Response header '" `mappend` CI.original hname `mappend` "': '" `mappend` hvalue `mappend` "'"
  responseConsumer request metadataRef hresp

-- | Run a URI-only AWS transaction. Returns a URI that can be sent anywhere. Does not work with all requests.
-- 
-- Usage:
-- @
--     uri <- awsUri cfg request
-- @
awsUri :: (SignQuery request, MonadBase IO io)
         => Configuration -> ServiceConfiguration request UriOnlyQuery -> request -> io B.ByteString
awsUri cfg info request = liftBase $ do
  let ti = timeInfo cfg
      cr = credentials cfg
  sd <- signatureData ti cr
  let q = signQuery request info sd
  logger cfg Debug $ T.pack $ "String to sign: " ++ show (sqStringToSign q)
  return $ queryToUri q

{-
-- | Run an iterated AWS transaction. May make multiple HTTP requests.
awsIteratedAll :: (IteratedTransaction r a)
                  => Configuration
                  -> ServiceConfiguration r NormalQuery
                  -> HTTP.Manager
                  -> r
                  -> ResourceT IO (Response [ResponseMetadata a] a)
awsIteratedAll cfg scfg manager req_ = go req_ Nothing
  where go request prevResp = do Response meta respAttempt <- aws cfg scfg manager request
                                 case maybeCombineIteratedResponse prevResp <$> respAttempt of
                                   f@(Failure _) -> return (Response [meta] f)
                                   s@(Success resp) -> 
                                     case nextIteratedRequest request resp of
                                       Nothing -> 
                                         return (Response [meta] s)
                                       Just nextRequest -> 
                                         mapMetadata (meta:) `liftM` go nextRequest (Just resp)
-}

awsIteratedSource :: (IteratedTransaction r a, DefaultServiceConfiguration (ServiceConfiguration r NormalQuery))
                     => Environment
                     -> r
                     -> C.Producer (ResourceT IO) (Response (ResponseMetadata a) a)
awsIteratedSource env req_ = go req_
  where go request = do resp <- lift $ aws env request
                        C.yield resp
                        case responseResult resp of
                          Left _  -> return ()
                          Right x ->
                            case nextIteratedRequest request x of
                              Nothing -> return ()
                              Just nextRequest -> go nextRequest

awsIteratedList :: (IteratedTransaction r a, ListResponse a i, DefaultServiceConfiguration (ServiceConfiguration r NormalQuery))
                     => Environment
                     -> r
                     -> C.Producer (ResourceT IO) i
awsIteratedList env req
  = awsIteratedSource env req
    C.=$=
    CL.concatMapM (fmap listResponse . readResponseIO)
