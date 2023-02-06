{-# LANGUAGE CPP                   #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE BangPatterns          #-}

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
, pureAws
, memoryAws
, simpleAws
  -- ** Unsafe runners
, unsafeAws
, unsafeAwsRef
  -- ** URI runners
, awsUri
  -- * Iterated runners
--, awsIteratedAll
, awsIteratedSource
, awsIteratedSource'
, awsIteratedList
, awsIteratedList'
)
where

import           Aws.Core
import           Control.Applicative
import           Control.Monad
import qualified Control.Monad.Catch          as E
import           Control.Monad.IO.Class
import           Control.Monad.Trans
import           Control.Monad.Trans.Resource
import qualified Data.ByteString              as B
import qualified Data.ByteString.Lazy         as L
import qualified Data.CaseInsensitive         as CI
import qualified Data.Conduit                 as C
import qualified Data.Conduit.List            as CL
import           Data.IORef
import           Data.Monoid
import qualified Data.Text                    as T
import qualified Data.Text.Encoding           as T
import qualified Data.Text.IO                 as T
import qualified Network.HTTP.Conduit         as HTTP
import qualified Network.HTTP.Client.TLS      as HTTP
import           System.IO                    (stderr)
import           Prelude

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
        timeInfo    :: TimeInfo
        -- | AWS access credentials.
      , credentials :: Credentials
        -- | The error / message logger.
      , logger      :: Logger
      , proxy       :: Maybe HTTP.Proxy
      }

-- | The default configuration, with credentials loaded from environment variable or configuration file
-- (see 'loadCredentialsDefault').
baseConfiguration :: MonadIO io => io Configuration
baseConfiguration = liftIO $ do
  cr <- loadCredentialsDefault
  case cr of
    Nothing -> E.throwM $ NoCredentialsException "could not locate aws credentials"
    Just cr' -> return Configuration {
                      timeInfo = Timestamp
                    , credentials = cr'
                    , logger = defaultLog Warning
                    , proxy = Nothing
                    }

-- | Debug configuration, which logs much more verbosely.
dbgConfiguration :: MonadIO io => io Configuration
dbgConfiguration = do
  c <- baseConfiguration
  return c { logger = defaultLog Debug }

-- | Run an AWS transaction, with HTTP manager and metadata wrapped in a 'Response'.
--
-- All errors are caught and wrapped in the 'Response' value.
--
-- Metadata is logged at level 'Info'.
--
-- Usage (with existing 'HTTP.Manager'):
-- @
--     resp <- aws cfg serviceCfg manager request
-- @
aws :: (Transaction r a)
      => Configuration
      -> ServiceConfiguration r NormalQuery
      -> HTTP.Manager
      -> r
      -> ResourceT IO (Response (ResponseMetadata a) a)
aws = unsafeAws

-- | Run an AWS transaction, with HTTP manager and metadata returned in an 'IORef'.
--
-- Errors are not caught, and need to be handled with exception handlers.
--
-- Metadata is not logged.
--
-- Usage (with existing 'HTTP.Manager'):
-- @
--     ref <- newIORef mempty;
--     resp <- awsRef cfg serviceCfg manager request
-- @

-- Unfortunately, the ";" above seems necessary, as haddock does not want to split lines for me.
awsRef :: (Transaction r a)
      => Configuration
      -> ServiceConfiguration r NormalQuery
      -> HTTP.Manager
      -> IORef (ResponseMetadata a)
      -> r
      -> ResourceT IO a
awsRef = unsafeAwsRef

-- | Run an AWS transaction, with HTTP manager and without metadata.
--
-- Metadata is logged at level 'Info'.
--
-- Usage (with existing 'HTTP.Manager'):
-- @
--     resp <- aws cfg serviceCfg manager request
-- @
pureAws :: (Transaction r a)
      => Configuration
      -> ServiceConfiguration r NormalQuery
      -> HTTP.Manager
      -> r
      -> ResourceT IO a
pureAws cfg scfg mgr req = readResponseIO =<< aws cfg scfg mgr req

-- | Run an AWS transaction, with HTTP manager and without metadata.
--
-- Metadata is logged at level 'Info'.
--
-- Usage (with existing 'HTTP.Manager'):
-- @
--     resp <- aws cfg serviceCfg manager request
-- @
memoryAws :: (Transaction r a, AsMemoryResponse a, MonadIO io)
      => Configuration
      -> ServiceConfiguration r NormalQuery
      -> HTTP.Manager
      -> r
      -> io (MemoryResponse a)
memoryAws cfg scfg mgr req = liftIO $ runResourceT $ loadToMemory =<< readResponseIO =<< aws cfg scfg mgr req

-- | Run an AWS transaction, /without/ HTTP manager and without metadata.
--
-- Metadata is logged at level 'Info'.
--
-- Note that this is potentially less efficient than using 'aws', because HTTP connections cannot be re-used.
--
-- Usage:
-- @
--     resp <- simpleAws cfg serviceCfg request
-- @
simpleAws :: (Transaction r a, AsMemoryResponse a, MonadIO io)
            => Configuration
            -> ServiceConfiguration r NormalQuery
            -> r
            -> io (MemoryResponse a)
simpleAws cfg scfg request = liftIO $ runResourceT $ do
    manager <- liftIO HTTP.getGlobalManager
    loadToMemory =<< readResponseIO =<< aws cfg scfg manager request

-- | Run an AWS transaction, without enforcing that response and request type form a valid transaction pair.
--
-- This is especially useful for debugging and development, you should not have to use it in production.
--
-- All errors are caught and wrapped in the 'Response' value.
--
-- Metadata is wrapped in the Response, and also logged at level 'Info'.
unsafeAws
  :: (ResponseConsumer r a,
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
      SignQuery r) =>
     Configuration -> ServiceConfiguration r NormalQuery -> HTTP.Manager -> IORef (ResponseMetadata a) -> r -> ResourceT IO a
unsafeAwsRef cfg info manager metadataRef request = do
  sd <- liftIO $ signatureData <$> timeInfo <*> credentials $ cfg
  let !q = {-# SCC "unsafeAwsRef:signQuery" #-} signQuery request info sd
  let logDebug = liftIO . logger cfg Debug . T.pack
  logDebug $ "String to sign: " ++ show (sqStringToSign q)
  !httpRequest <- {-# SCC "unsafeAwsRef:httpRequest" #-} liftIO $ do
    req <- queryToHttpRequest q
    return $ req { HTTP.proxy = proxy cfg }
  logDebug $ "Host: " ++ show (HTTP.host httpRequest)
  logDebug $ "Path: " ++ show (HTTP.path httpRequest)
  logDebug $ "Query string: " ++ show (HTTP.queryString httpRequest)
  logDebug $ "Header: " ++ show (HTTP.requestHeaders httpRequest)
  case HTTP.requestBody httpRequest of
    HTTP.RequestBodyLBS lbs -> logDebug $ "Body: " ++ show (L.take 1000 lbs)
    HTTP.RequestBodyBS bs -> logDebug $ "Body: " ++ show (B.take 1000 bs)
    _ -> return ()
  hresp <- {-# SCC "unsafeAwsRef:http" #-} HTTP.http httpRequest manager
  logDebug $ "Response status: " ++ show (HTTP.responseStatus hresp)
  forM_ (HTTP.responseHeaders hresp) $ \(hname,hvalue) -> liftIO $
    logger cfg Debug $ T.decodeUtf8 $ "Response header '" `mappend` CI.original hname `mappend` "': '" `mappend` hvalue `mappend` "'"
  {-# SCC "unsafeAwsRef:responseConsumer" #-} responseConsumer httpRequest request metadataRef hresp

-- | Run a URI-only AWS transaction. Returns a URI that can be sent anywhere. Does not work with all requests.
--
-- Usage:
-- @
--     uri <- awsUri cfg request
-- @
awsUri :: (SignQuery request, MonadIO io)
         => Configuration -> ServiceConfiguration request UriOnlyQuery -> request -> io B.ByteString
awsUri cfg info request = liftIO $ do
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

awsIteratedSource
    :: (IteratedTransaction r a)
    => Configuration
    -> ServiceConfiguration r NormalQuery
    -> HTTP.Manager
    -> r
    -> forall i. C.ConduitT i (Response (ResponseMetadata a) a) (ResourceT IO) ()
awsIteratedSource cfg scfg manager req_ = awsIteratedSource' run req_
  where
    run r = do
        res <- aws cfg scfg manager r
        a <- readResponseIO res
        return (a, res)


awsIteratedList
    :: (IteratedTransaction r a, ListResponse a i)
    => Configuration
    -> ServiceConfiguration r NormalQuery
    -> HTTP.Manager
    -> r
    -> forall j. C.ConduitT j i (ResourceT IO) ()
awsIteratedList cfg scfg manager req = awsIteratedList' run req
  where
    run r = readResponseIO =<< aws cfg scfg manager r


-------------------------------------------------------------------------------
-- | A more flexible version of 'awsIteratedSource' that uses a
-- user-supplied run function. Useful for embedding AWS functionality
-- within application specific monadic contexts.
awsIteratedSource'
    :: (Monad m, IteratedTransaction r a)
    => (r -> m (a, b))
    -- ^ A runner function for executing transactions.
    -> r
    -- ^ An initial request
    -> forall i. C.ConduitT i b m ()
awsIteratedSource' run r0 = go r0
    where
      go q = do
          (a, b) <- lift $ run q
          C.yield b
          case nextIteratedRequest q a of
            Nothing -> return ()
            Just q' -> go q'


-------------------------------------------------------------------------------
-- | A more flexible version of 'awsIteratedList' that uses a
-- user-supplied run function. Useful for embedding AWS functionality
-- within application specific monadic contexts.
awsIteratedList'
    :: (Monad m, IteratedTransaction r b, ListResponse b c)
    => (r -> m b)
    -- ^ A runner function for executing transactions.
    -> r
    -- ^ An initial request
    -> forall i. C.ConduitT i c m ()
awsIteratedList' run r0 =
    awsIteratedSource' run' r0 `C.fuse`
    CL.concatMap listResponse
  where
    dupl a = (a,a)
    run' r = dupl `liftM` run r
