-- ------------------------------------------------------ --
-- Copyright © 2012 AlephCloud Systems, Inc.
-- ------------------------------------------------------ --

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import Prelude hiding (lookup)

import System.IO (hPutStrLn, stderr)

import Network.HTTP.Conduit (simpleHttp)
import qualified Data.ByteString.Lazy.Char8 as B8 (unpack)
import Data.Text (unpack, pack, append)
import Data.Text.Encoding (encodeUtf8)
import Data.Attempt
import Data.IP (IPv4)
import Control.Applicative ((<$>))
import Control.Concurrent (threadDelay)
import Control.Monad.IO.Class (liftIO)

import Network.DNS (lookup, makeResolvSeed, defaultResolvConf, withResolver, TYPE(A), RDATA(RD_A))

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
                } deriving (Show, Data, Typeable)

dyndnsargs :: DynDnsArgs
dyndnsargs = DynDnsArgs
           { ttl = 60 &= help "The time to live header for the A record of the subdomain" &= typ "SECONDS"
           , sleep = 60 &= help "The time to sleep after each check and possible reset of the A record of the subdomain" &= typ "SECONDS"
           , retry = 4 &= help "The number of times network requests are retried" &= typ "INTEGER"
           , retry_sleep = 1 &= help "The time to wait between two retries" &= typ "SECONDS"
           , hosted_zone = def &= argPos 0 &= typ "HostedZone" -- &= help "The domain of the Route53 hosted zone" &= typ "ABSOLUTE DNSNAME"
           , subdomain = def &= argPos 1 &= typ "SubDomain" -- &= help "The subdomain (relative to hosted zone domain)" &= typ "RELATIVE DNSNAME"
           }
           &= verbosity
           &= program "DynDNS"
           &= summary "DynDNS 0.1, © 2012 AlephCloud System, Inc."
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
            }

logError :: String -> IO ()
logError = hPutStrLn stderr

logNormal :: String -> IO ()
logNormal = whenNormal . putStrLn

logVerbose :: String -> IO ()
logVerbose = whenLoud . putStrLn

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
                logError $ "WARNING: DNS lookup for " ++ unpack dom ++ " without result."
                return []

    where
    dom = dText . confDomain $ conf
    toDnsDomain = encodeUtf8
    ret = Utils.retry (confRetrySleep conf) (confRetry conf)

check :: Config -> [IPv4] -> IO ()

check conf [ip] = do
    rip <- realip
    logVerbose $ "Current public visible IP of the local machine is " ++ show rip ++ "."
    
    ip' <- if ip == rip 
        then return [ip]
        else do
            logNormal $ "INFO: real IPv4 address does not match the DNS IPv4 address."
            sip <- setip conf rip
            case sip of
                Failure _ -> return [ip]
                Success _ -> return [rip]
    threadDelay $ (confSleep conf) * 1000000
    check conf ip'

check conf _ = do
    logVerbose $ "No valid single IPv4 address (A record) in DNS." 
    
    rip <- realip
    logVerbose $ "Current public visible IP of the local machine is " ++ show rip ++ "."
    
    sip <- setip conf rip
    ip' <- case sip of
        Failure _ -> do
            logError $ "WARNING: Failed to update ip address for " ++ unpack dom ++ " to " ++ show rip ++ "."
            return []
        Success _ -> do
            logNormal $ "INFO: Successfully updated ip address for " ++ unpack dom ++ " to " ++ show rip ++ "."
            return [rip]
    threadDelay $ (confSleep conf) * 1000000
    check conf ip'
    where
    dom = dText (confDomain conf)

setip :: Config -> IPv4 -> IO (Attempt [ChangeResourceRecordSetsResponse])
setip conf ip = runAttemptT $ do
    zid <- ret . AttemptT . liftIO $ getZoneIdByName (confHostedZone conf)
    ret . AttemptT . liftIO $ setARecordRetry zid (confDomain conf) (confTtl conf) ip
    where
    ret = Utils.retry (confRetrySleep conf) (confRetry conf)

-- -------------------------------------------------------------------------- --
-- Main
--
main :: IO ()
main = do
    a <- cmdArgs dyndnsargs

    let conf = Config
             { confHostedZone = Domain . pack . hosted_zone $ a
             , confDomain = Domain $ (pack (subdomain a)) `append` "." `append` (pack (hosted_zone a))
             , confTtl = ttl a
             , confSleep = sleep a
             , confRetry = retry a
             , confRetrySleep = retry_sleep a
             }

    let dom        = dText (confDomain conf)
        hostedzone = dText (confHostedZone conf)

    logNormal $ "Start DynDNS client for domain " ++ unpack dom ++ " in hosted zone " ++ unpack hostedzone ++ "."
    dip <- dnsip conf

    logNormal $ "Current IPv4 address (A record) in DNS is " ++ show dip ++ "." 
    check conf dip

