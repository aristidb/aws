-- ------------------------------------------------------ --
-- Copyright © 2012 AlephCloud Systems, Inc.
-- ------------------------------------------------------ --

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import Prelude hiding (lookup)

import System.IO (stderr, Handle, withFile, BufferMode(LineBuffering), IOMode(AppendMode), hSetBuffering)
import System.Locale (defaultTimeLocale)

import Network.HTTP.Conduit (simpleHttp)
import qualified Data.ByteString.Lazy.Char8 as B8 (unpack)
import Data.Text (Text, pack, append)
import Data.Text.IO (hPutStrLn)
import Data.Text.Encoding (encodeUtf8)
import Data.Attempt
import Data.Monoid ((<>))
import Data.IP (IPv4)
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (formatTime)
import Control.Monad (when)
import Control.Applicative ((<$>))
import Control.Concurrent (threadDelay)
import Control.Monad.IO.Class (liftIO)

import Network.DNS (lookup, makeResolvSeed, defaultResolvConf, withResolver, TYPE(A), RDATA(RD_A))

import qualified Aws as Aws
import Aws (LogLevel(..), Logger)
import Aws.Route53 (Domain(..), ChangeResourceRecordSetsResponse)

import Utils hiding (retry)
import qualified Utils (retry)
import AttemptT

import System.Console.CmdArgs

-- -------------------------------------------------------------------------- --
-- Command Line Arguments and Configuration
--
data DynDnsArgs = DynDnsArgs 
                { hosted_zone :: String
                , subdomain :: String
                , ttl :: Int
                , sleep :: Int
                , retry :: Int
                , retry_sleep :: Int
                , log_file :: FilePath
                , aws_keys_file :: FilePath
                , aws_key :: String
                } deriving (Show, Data, Typeable)

dyndnsargs :: DynDnsArgs
dyndnsargs = DynDnsArgs
           { ttl = 60 &= help "Value of the time to live header for the A record of the subdomain (default: 60)" &= typ "SECONDS"
           , sleep = 60 &= help "Time to sleep after each check and possible reset of the A record of the subdomain (default: 60)" &= typ "SECONDS"
           , retry = 4 &= help "Number of times network requests are retried (default: 4)" &= typ "INTEGER"
           , retry_sleep = 1 &= help "Time to wait between two retries (defaul: 1)" &= typ "SECONDS"
           , aws_keys_file = def &= help "File with the AWS access keys (default: ~/.aws-keys)" &= typFile
           , aws_key = def &= help "Aws key to use (default: default)" &= typ "STRING"
           , log_file = def &= help "File with where the logs will be written to (default: stderr)" &= typFile
           , hosted_zone = def &= argPos 0 &= typ "HostedZone"
           , subdomain = def &= argPos 1 &= typ "SubDomain"
           }
           &= verbosity
           &= program "r53-dyndns"
           &= summary "r53-dyndns 0.1, © 2012 AlephCloud System, Inc."
           &= help "Regulary check and set the A record of the DNS name local machine to the effective public IP address"
           &= details [ "Uses AWS Route53 as DNS server backend."
                      , "An Route53 account is need with a configured hosted zone."
                      , "The subdomain must be choosen for the hosted zone."
                      , "The Haskell AWS package must be configured with the default access key in place."
                      , ""
                      , "You must provide the domain of the hosted zone as absolute DNS name (ends with a dot)"
                      , "and the subdomain relative to the hosted zone domain."
                      ]

data Config = Config
            { confHostedZone :: Domain
            , confDomain :: Domain
            , confTtl :: Int
            , confSleep :: Int
            , confRetry :: Int
            , confRetrySleep :: Int
            , confAws :: Aws.Configuration
            , confLog :: Logger
            }

-- We reuse the simple logging approach from the Aws package.
-- It is not very efficient but for this application we do not need
-- to worry about performance
getLogger :: Handle -> LogLevel -> Logger
getLogger h minlevel level msg = do 
    when (level >= minlevel) $ do 
        time <- formatTime defaultTimeLocale "%F %X" <$> getCurrentTime
        let m = pack time <> " - " <> pack (show level) <> ": " <> msg
        hPutStrLn h m

logDebug :: Config -> Text -> IO ()
logDebug conf msg = (confLog conf) Debug msg

logInfo :: Config -> Text -> IO ()
logInfo conf msg = (confLog conf) Info msg

logWarning :: Config -> Text -> IO ()
logWarning conf msg = (confLog conf) Warning msg

logError :: Config -> Text -> IO ()
logError conf msg = (confLog conf) Error msg

-- -------------------------------------------------------------------------- --
-- Utils
--
realip :: IO IPv4
realip = read . B8.unpack <$> simpleHttp  "http://api.externalip.net/ip/"

-- TODO Retry depneding on the error. Do not retry on startup.
dnsip :: Config -> IO [IPv4]
dnsip conf = do
    rs <- makeResolvSeed defaultResolvConf
    withResolver rs $ \resolver -> do
        result <- ret $ lookup resolver (toDnsDomain dom) A
        case result of
            Just ips -> return . map (\(RD_A ip) -> ip) $ ips
            Nothing -> do
                logWarning conf $ "DNS lookup for " <> dom <> " without result."
                return []

    where
    dom = dText . confDomain $ conf
    toDnsDomain = encodeUtf8
    ret = Utils.retry (confRetrySleep conf) (confRetry conf)

check :: Config -> [IPv4] -> IO ()

check conf [ip] = do
    rip <- realip
    logDebug conf $ "Current public visible IP of the local machine is " <> pack (show rip) <> "."
    
    ip' <- if ip == rip 
        then return [ip]
        else do
            logInfo conf $ "Real IPv4 address does not match the DNS IPv4 address."
            sip <- setip conf rip
            case sip of
                Failure _ -> return [ip]
                Success _ -> return [rip]
    threadDelay $ (confSleep conf) * 1000000
    check conf ip'

check conf _ = do
    logDebug conf $ "No valid single IPv4 address (A record) in DNS." 
    
    rip <- realip
    logDebug conf $ "Current public visible IP of the local machine is " <> pack (show rip) <> "."
    
    sip <- setip conf rip
    ip' <- case sip of
        Failure _ -> do
            logWarning conf $ "Failed to update ip address for " <> dom <> " to " <> pack (show rip) <> "."
            return []
        Success _ -> do
            logInfo conf $ "Successfully updated ip address for " <> dom <> " to " <> pack (show rip) <> "."
            return [rip]
    threadDelay $ (confSleep conf) * 1000000
    check conf ip'
    where
    dom = dText (confDomain conf)

setip :: Config -> IPv4 -> IO (Attempt [ChangeResourceRecordSetsResponse])
setip conf ip = runAttemptT $ do
    zid <- ret . AttemptT . liftIO $ getZoneIdByName awsconf (confHostedZone conf)
    ret . AttemptT . liftIO $ setARecordRetry awsconf zid (confDomain conf) (confTtl conf) ip
    where
    ret = Utils.retry (confRetrySleep conf) (confRetry conf)
    awsconf = confAws conf

-- -------------------------------------------------------------------------- --
-- Main
--

awsConfiguration :: DynDnsArgs -> Logger -> IO Aws.Configuration
awsConfiguration a logger = do
    maybeCreds <- awsCredentials (aws_keys_file a) (aws_key a)
    creds <- case maybeCreds of
        Nothing -> do logger Error $ "Failed to load AWS Credentials."
                      error "FATAL ERROR: Failed to load AWS Credentials."
        Just x  -> return x
    return $ Aws.Configuration
           { Aws.timeInfo    = Aws.Timestamp
           , Aws.credentials = creds
           , Aws.logger      = logger
           }
    where
    awsCredentials file key | file == def && key == def = Aws.loadCredentialsDefault
                            | key == def                = Aws.loadCredentialsFromFile file Aws.credentialsDefaultKey
                            | file == def               = do f <- Aws.credentialsDefaultFile
                                                             Aws.loadCredentialsFromEnvOrFile f (pack key)
                            | otherwise                 = Aws.loadCredentialsFromFile file (pack key)

main :: IO ()
main = do
    a <- cmdArgs dyndnsargs
    

    let execWithLogfile logfile = do
        hSetBuffering logfile LineBuffering

        verb <- getVerbosity
        let loglevel = case verb of
                Quiet  -> Error
                Normal -> Warning
                Loud   -> Aws.Debug

        let logger = getLogger logfile loglevel
        logger Info $ "Startup r53-dyndns"

        awsConf <- awsConfiguration a logger

        let conf = Config
                 { confHostedZone = Domain . pack . hosted_zone $ a
                 , confDomain = Domain $ (pack (subdomain a)) `append` "." `append` (pack (hosted_zone a))
                 , confTtl = ttl a
                 , confSleep = sleep a
                 , confRetry = retry a
                 , confRetrySleep = retry_sleep a
                 , confAws = awsConf
                 , confLog = logger
                 }

        let dom        = dText (confDomain conf)
            hostedzone = dText (confHostedZone conf)

        logInfo conf $ "Start r53-dyndns client for domain " <> dom <> " in hosted zone " <> hostedzone <> "."
        dip <- dnsip conf

        logInfo conf $ "Current IPv4 address (A record) in DNS is " <> pack (show dip) <> "." 
        check conf dip

    if log_file a /= def
       then withFile (log_file a) AppendMode execWithLogfile
       else execWithLogfile stderr

