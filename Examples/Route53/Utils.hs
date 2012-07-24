-- ------------------------------------------------------ --
-- Copyright © 2012 AlephCloud Systems, Inc.
-- ------------------------------------------------------ --

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Utils where

import Data.Text                 (Text, pack)
import Data.List                 (find)
import Data.Maybe                (fromJust, listToMaybe)
import Data.Attempt              (Attempt(..))

import Control.Monad             (MonadPlus, mzero, mplus)
import Control.Applicative       ((<$>))
import Control.Monad.IO.Class    (MonadIO, liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.List  (runListT)
import Control.Concurrent        (threadDelay)

import Network.HTTP.Conduit      (Manager, withManager)
import Data.IP                   (IPv4)

import Aws                       (aws, Response(..), Transaction, DefaultServiceConfiguration, 
                                  ServiceConfiguration, defServiceConfig, ResponseMetadata, 
                                  awsIteratedAll, Configuration)
import Aws.Core                  (NormalQuery, IteratedTransaction)
import Aws.Route53

import AttemptT

-- -------------------------------------------------------------------------- --
-- Request Utils

-- | extract result of an 'Attempt' from a 'Response'
--
getResult :: Response m a -> Attempt a
getResult (Response _ a) = a

-- | Make a request using the base configuration and the default
--   service configuration.
--
makeDefaultRequest :: ( Transaction r a
                      , Functor m
                      , MonadIO m
                      , DefaultServiceConfiguration (ServiceConfiguration r NormalQuery)
                      ) 
                   => Configuration -> Manager -> r -> m (Response (ResponseMetadata a) a)
makeDefaultRequest cfg manager request = do
    let scfg = defServiceConfig
    aws cfg scfg manager request

-- | Make an iterated request using the base configuration and the default
--   service configuration.
--
makeDefaultRequestAll :: ( IteratedTransaction r a
                         , Functor m
                         , MonadIO m
                         , DefaultServiceConfiguration (ServiceConfiguration r NormalQuery)
                         ) 
                      => Configuration -> Manager -> r -> m (Response [ResponseMetadata a] a)
makeDefaultRequestAll cfg manager request = do
    let scfg = defServiceConfig
    awsIteratedAll cfg scfg manager request

-- | Executes the given request using the default configuration and a fresh 
--   connection manager. Extracts the enclosed response body and returns it 
--   within the IO monad.
--
makeSingleRequest :: ( Transaction r a
                     , Show r
                     , DefaultServiceConfiguration (ServiceConfiguration r NormalQuery)
                     ) 
                  => Configuration -> r -> IO (Attempt a)
makeSingleRequest cfg r = do
    getResult <$> (withManager (\m -> makeDefaultRequest cfg m r))

-- | Executes the given iterated request using the default configuration and a fresh 
--   connection manager. Extracts the enclosed response body and returns it 
--   within the IO monad.
--
makeSingleRequestAll :: ( IteratedTransaction r a
                        , Show r
                        , DefaultServiceConfiguration (ServiceConfiguration r NormalQuery)
                        ) 
                     => Configuration -> r -> IO (Attempt a)
makeSingleRequestAll cfg r = do
    getResult <$> (withManager (\m -> makeDefaultRequestAll cfg m r))

-- | Given a Changeid returns the change info status for the corresponding 
--   request.
--
getChangeStatus :: Configuration -> ChangeId -> IO (Attempt ChangeInfoStatus)
getChangeStatus cfg changeId = 
    fmap (ciStatus . gcrChangeInfo) <$> (makeSingleRequest cfg $ getChange changeId)

-- | Extracts the ChangeId from a response using the given function to extract
--   the ChangeInfo from the response.
--
getChangeId :: Functor f => (a -> ChangeInfo) -> f a -> f ChangeId
getChangeId changeInfoExtractor response = ciId . changeInfoExtractor <$> response

-- | Example usage of getChangeId.
--
getChangeResourceRecordSetsResponseChangeId :: Functor f => f ChangeResourceRecordSetsResponse -> f ChangeId
getChangeResourceRecordSetsResponseChangeId response = getChangeId crrsrChangeInfo response

-- TODO implement wait for INSYNC

-- -------------------------------------------------------------------------- --
-- Hosted Zones

-- | Get all hosted zones of the user.
--
getAllZones :: Configuration -> IO (Attempt HostedZones)
getAllZones cfg = fmap lhzrHostedZones <$> makeSingleRequestAll cfg listHostedZones

-- | Get a hosted zone by its 'HostedZoneId'.
--
getZoneById :: Configuration -> HostedZoneId -> IO (Attempt HostedZone)
getZoneById cfg hzid = fmap ghzrHostedZone <$> makeSingleRequest cfg (getHostedZone hzid)

-- | Get a hosted zone by its domain name.
--   
--   Results in an error if no hosted zone exists for the given domain name.
--
getZoneByName :: Configuration -> Domain -> IO (Attempt HostedZone)
getZoneByName cfg z = fmap (fromJust . find ((z==) . hzName)) <$> getAllZones cfg

-- | Returns the hosted zone id of the hosted zone for the given domain.
--
getZoneIdByName :: Configuration -> Domain -> IO (Attempt HostedZoneId)
getZoneIdByName cfg hzName = fmap hzId <$> getZoneByName cfg hzName

-- -------------------------------------------------------------------------- --
-- Resource Records Sets

-- | Simplified construction for a ResourceRecordSet.
--
simpleResourceRecordSet :: Domain -> RecordType -> Int -> Text -> ResourceRecordSet
simpleResourceRecordSet domain rtype ttl value = 
    ResourceRecordSet domain rtype Nothing Nothing Nothing Nothing (Just ttl) [(ResourceRecord value)]

-- | Returns the resource record sets in the hosted zone with the given domain
--   name.
--
--   Note the 'zName' is the domain name of the hosted zone itself.
--
getResourceRecordSetsByHostedZoneName :: Configuration -> Domain -> IO (Attempt ResourceRecordSets)
getResourceRecordSetsByHostedZoneName cfg zName = do
    attemptHzid <- getZoneIdByName cfg zName
    case attemptHzid of
        Success hzid -> fmap lrrsrResourceRecordSets <$> makeSingleRequestAll cfg (listResourceRecordSets hzid)
        Failure e -> return $ Failure e

-- | Lists all resource record sets in the hosted zone with the given hosted 
--   zone id.
--
getResourceRecordSets :: Configuration -> HostedZoneId -> IO (Attempt ResourceRecordSets)
getResourceRecordSets cfg hzid = 
    fmap lrrsrResourceRecordSets <$> makeSingleRequestAll cfg (listResourceRecordSets hzid)

-- | Lists all resource record sets in the given hosted zone for the given 
--   domain.
--
getResourceRecordSetsByDomain :: Configuration -> HostedZoneId -> Domain -> IO (Attempt ResourceRecordSets)
getResourceRecordSetsByDomain cfg hzid domain = do
    let req = (listResourceRecordSets hzid) { lrrsName = Just domain }
    fmap lrrsrResourceRecordSets <$> makeSingleRequestAll cfg req

-- | Returns all resource records sets in the hosted zone with the given hosted
--   zone id for the given DNS record type.
--
getResourceRecordSetsByType :: Configuration -> HostedZoneId -> RecordType -> IO (Attempt ResourceRecordSets)
getResourceRecordSetsByType cfg hzid dnsRecordType = 
    fmap (filter ((== dnsRecordType) . rrsType)) <$> getResourceRecordSets cfg hzid

-- | Returns the resource record set of the given type for the given domain in 
--   the given hosted zone.
--
getResourceRecords :: Configuration -> HostedZoneId -> Domain -> RecordType -> IO (Attempt (Maybe ResourceRecordSet))
getResourceRecords cfg cid domain rtype = do
    let req = ListResourceRecordSets cid (Just domain) (Just rtype) Nothing (Just 1)
    fmap (listToMaybe . lrrsrResourceRecordSets) <$> (makeSingleRequest cfg $ req)

-- | Updates the resouce records of the given type for the given domain in the 
--   given hosted zone using the given mapping function.
--
--   Recall that the functions in this module are example usages of the 
--   Aws.Route53 module. In a production environment one would reuse the same 
--   connection manager and configuration for all involved requests.
--
modifyRecords :: Configuration
              -> HostedZoneId 
              -> Domain 
              -> RecordType 
              -> ([ResourceRecord] -> [ResourceRecord]) 
              -> IO (Attempt (ChangeResourceRecordSetsResponse, ChangeResourceRecordSetsResponse))
modifyRecords cfg cid domain rtype f = runAttemptT $ do
    -- Fixme fail more gracefully
    Just (rrs:: ResourceRecordSet) <- AttemptT . liftIO $ getResourceRecords cfg cid domain rtype
    let rrs' = rrs { rrsRecords = f (rrsRecords rrs) }
    
    -- Handle errors gracefully. What if we fail in the middle?
    (r1 :: ChangeResourceRecordSetsResponse) <- AttemptT . liftIO . makeSingleRequest cfg $ ChangeResourceRecordSets cid Nothing [(DELETE, rrs)] :: AttemptT IO (ChangeResourceRecordSetsResponse)
    r2 <- AttemptT . liftIO . makeSingleRequest cfg $ ChangeResourceRecordSets cid Nothing [(CREATE, rrs')]
    return (r1, r2)

-- | Updates the A record for the given domain in the given zone to the given 
--   IP address (encoded as Text).
--
setARecord :: Configuration
           -> HostedZoneId -- ^ Zone ID
           -> Domain       -- ^ Domain
           -> Int          -- ^ TTL for the record
           -> IPv4         -- ^ The new value for the A record, an IPv4 address
           -> IO (Attempt [ChangeResourceRecordSetsResponse])
setARecord cfg cid domain ttl ip = runAttemptT $ do
       maybeRrs <- AttemptT . liftIO $ getResourceRecords cfg cid domain A
       runListT $ case maybeRrs of
           Just rrs -> lift $ AttemptT . liftIO . makeSingleRequest cfg $ ChangeResourceRecordSets cid Nothing [(DELETE, rrs)]
           Nothing -> mzero
        `mplus` do
            let rr = simpleResourceRecordSet domain A ttl (pack . show $ ip)
            lift $ AttemptT . liftIO . makeSingleRequest cfg $ ChangeResourceRecordSets cid Nothing [(CREATE, rr)]

retry :: (MonadIO m, MonadPlus m) => Int -> Int -> m a -> m a
retry _ 1 req = req
retry pause num req | num < 0   = error $ "Illegal argument to retry. Expected positive Int, got " ++ (show num)
                    | otherwise = req `mplus` (wait >> retry pause (num-1) req)
    where
    wait = liftIO . threadDelay $ (pause * 1000000)


-- | Updates the A record for the given domain in the given zone to the given 
--   IP address (encoded as Text).
--
setARecordRetry :: Configuration
                -> HostedZoneId -- ^ Zone ID
                -> Domain       -- ^ Domain
                -> Int          -- ^ TTL for the record
                -> IPv4         -- ^ The new value for the A record, an IPv4 address
                -> IO (Attempt [ChangeResourceRecordSetsResponse])
setARecordRetry cfg cid domain ttl ip = runAttemptT $ do
       maybeRrs <- r . AttemptT . liftIO $ getResourceRecords cfg cid domain A
       runListT $ case maybeRrs of
           Just rrs -> lift . r . AttemptT . liftIO . makeSingleRequest cfg $ ChangeResourceRecordSets cid Nothing [(DELETE, rrs)]
           Nothing -> mzero
        `mplus` do
            let rr = simpleResourceRecordSet domain A ttl (pack . show $ ip)
            lift . r . AttemptT . liftIO . makeSingleRequest cfg $ ChangeResourceRecordSets cid Nothing [(CREATE, rr)]

    where
    r = retry 1 4
