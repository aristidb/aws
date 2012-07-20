-- ------------------------------------------------------ --
-- Copyright © 2012 AlephCloud Systems, Inc.
-- ------------------------------------------------------ --

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}

module Route53.Samples where

import Data.Text                 (Text)
import Data.List                 (find)
import Data.Maybe                (fromJust, listToMaybe)
import Data.Attempt              (Attempt(..), fromAttempt)

import Control.Monad             (mzero, mplus)
import Control.Applicative       ((<$>))
import Control.Monad.IO.Class    (MonadIO, liftIO)
import Control.Monad.Trans.Maybe (runMaybeT)
import Control.Monad.Trans.List  (runListT)

import Network.HTTP.Conduit      (Manager, withManager)

import Aws                       (aws, Response(..), Transaction, DefaultServiceConfiguration, 
                                  ServiceConfiguration, defServiceConfig, baseConfiguration,
                                  ResponseMetadata, awsIteratedAll)
import Aws.Core                  (NormalQuery, IteratedTransaction)
import Aws.Route53

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
                   => Manager -> r -> m (Response (ResponseMetadata a) a)
makeDefaultRequest manager request = do
    cfg <- baseConfiguration
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
                      => Manager -> r -> m (Response [ResponseMetadata a] a)
makeDefaultRequestAll manager request = do
    cfg <- baseConfiguration
    let scfg = defServiceConfig
    awsIteratedAll cfg scfg manager request

-- | Executes the given request using the default configuration and a fresh 
--   connection manager. Extracts the enclosed response body and returns it 
--   within the IO monad.
--
--   The result is wrapped in an 'Attempt'.
--
makeSingleRequest :: (Transaction r a
                     , Show r
                     , DefaultServiceConfiguration (ServiceConfiguration r NormalQuery)
                     ) 
                  => r -> IO a
makeSingleRequest r = do
    fromAttempt =<< getResult <$> (withManager (\m -> makeDefaultRequest m r))

-- | Executes the given iterated request using the default configuration and a fresh 
--   connection manager. Extracts the enclosed response body and returns it 
--   within the IO monad.
--
--   The result is wrapped in an 'Attempt'.
--
makeSingleRequestAll :: (IteratedTransaction r a
                        , Show r
                        , DefaultServiceConfiguration (ServiceConfiguration r NormalQuery)
                        ) 
                     => r -> IO a
makeSingleRequestAll r = do
    fromAttempt =<< getResult <$> (withManager (\m -> makeDefaultRequestAll m r))

-- | Given a Changeid returns the change info status for the corresponding 
--   request.
--
getChangeStatus :: ChangeId -> IO ChangeInfoStatus
getChangeStatus changeId = 
    ciStatus . gcrChangeInfo <$> (makeSingleRequest $ getChange changeId)

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
getAllZones :: IO HostedZones
getAllZones = lhzrHostedZones <$> makeSingleRequestAll listHostedZones

-- | Get a hosted zone by its 'HostedZoneId'.
--
getZoneById :: HostedZoneId -> IO HostedZone
getZoneById hzid = ghzrHostedZone <$> makeSingleRequest (getHostedZone hzid)

-- | Get a hosted zone by its domain name.
--   
--   Results in an error if no hosted zone exists for the given domain name.
--
getZoneByName :: Domain -> IO HostedZone
getZoneByName z = fromJust . find ((z==) . hzName) <$> getAllZones

-- | Returns the hosted zone id of the hosted zone for the given domain.
--
getZoneIdByName :: Domain -> IO HostedZoneId
getZoneIdByName hzName = hzId <$> getZoneByName hzName

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
getResourceRecordSetsByHostedZoneName :: Domain -> IO ResourceRecordSets
getResourceRecordSetsByHostedZoneName zName = do
    hzid <- getZoneIdByName zName
    lrrsrResourceRecordSets <$> makeSingleRequestAll (listResourceRecordSets hzid)

-- | Lists all resource record sets in the hosted zone with the given hosted 
--   zone id.
--
getResourceRecordSets :: HostedZoneId -> IO ResourceRecordSets
getResourceRecordSets hzid = 
    lrrsrResourceRecordSets <$> makeSingleRequestAll (listResourceRecordSets hzid)

-- | Lists all resource record sets in the given hosted zone for the given 
--   domain.
--
getResourceRecordSetsByDomain :: HostedZoneId -> Domain -> IO ResourceRecordSets
getResourceRecordSetsByDomain hzid domain = do
    let req = (listResourceRecordSets hzid) { lrrsName = Just domain }
    lrrsrResourceRecordSets <$> makeSingleRequestAll req

-- | Returns all resource records sets in the hosted zone with the given hosted
--   zone id for the given DNS record type.
--
getResourceRecordSetsByType :: HostedZoneId -> RecordType -> IO ResourceRecordSets
getResourceRecordSetsByType hzid dnsRecordType = 
    filter ((== dnsRecordType) . rrsType) <$> getResourceRecordSets hzid

-- | Returns the resource record set of the given type for the given domain in 
--   the given hosted zone.
--
getResourceRecords :: HostedZoneId -> Domain -> RecordType -> IO (Maybe ResourceRecordSet)
getResourceRecords cid domain rtype = do
    let req = ListResourceRecordSets cid (Just domain) (Just rtype) Nothing (Just 1)
    listToMaybe . lrrsrResourceRecordSets <$> (makeSingleRequest $ req)

-- | Updates the resouce records of the given type for the given domain in the 
--   given hosted zone using the given mapping function.
--
--   Recall that the functions in this module are example usages of the 
--   Aws.Route53 module. In a production environment one would reuse the same 
--   connection manager and configuration for all involved requests.
--
modifyRecords :: HostedZoneId 
              -> Domain 
              -> RecordType 
              -> ([ResourceRecord] -> [ResourceRecord]) 
              -> IO (Maybe (ChangeResourceRecordSetsResponse, ChangeResourceRecordSetsResponse))
modifyRecords cid domain rtype f = runMaybeT $ do
    -- Fixme fail more gracefully
    Just rrs <- liftIO $ getResourceRecords cid domain rtype
    let rrs' = rrs { rrsRecords = f (rrsRecords rrs) }
    
    -- Handle errors gracefully. What if we fail in the middle?
    r1 <- liftIO . makeSingleRequest $ ChangeResourceRecordSets cid Nothing [(DELETE, rrs)]
    r2 <- liftIO . makeSingleRequest $ ChangeResourceRecordSets cid Nothing [(CREATE, rrs')]
    return (r1, r2)

-- | Updates the A record for the given domain in the given zone to the given 
--   IP address (encoded as Text).
--
setARecord :: HostedZoneId -- ^ Zone ID
           -> Domain       -- ^ Domain
           -> Int          -- ^ TTL for the record
           -> Text         -- ^ The new value for the A record, an IPv4 address
           -> IO [ChangeResourceRecordSetsResponse]
setARecord cid domain ttl ip = runListT $ do
       maybeRrs <- liftIO $ getResourceRecords cid domain A
       case maybeRrs of
           Just rrs -> liftIO $ makeSingleRequest $ ChangeResourceRecordSets cid Nothing [(DELETE, rrs)]
           Nothing -> mzero
        `mplus` do
            let rr = simpleResourceRecordSet domain A ttl ip
            liftIO . makeSingleRequest $ ChangeResourceRecordSets cid Nothing [(CREATE, rr)]
