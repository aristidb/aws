-- ------------------------------------------------------ --
-- Copyright © 2012 AlephCloud Systems, Inc.
-- ------------------------------------------------------ --

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Utils where

import Data.Text                 (Text, pack, unpack)
import Data.List                 (find)
import Data.Attempt              (Attempt(..))

import Control.Monad             (MonadPlus, mplus)
import Control.Applicative       (Applicative, (<$>))
import Control.Monad.IO.Class    (MonadIO, liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.List  (runListT, ListT(..))
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
                     , MonadIO m
                     ) 
                  => Configuration -> r -> AttemptT m a
makeSingleRequest cfg r = do
     AttemptT . liftIO $ getResult <$> withManager (\m -> makeDefaultRequest cfg m r)

-- | Executes the given iterated request using the default configuration and a fresh 
--   connection manager. Extracts the enclosed response body and returns it 
--   within the IO monad.
--
makeSingleRequestAll :: ( IteratedTransaction r a
                        , Show r
                        , DefaultServiceConfiguration (ServiceConfiguration r NormalQuery)
                        , MonadIO m
                        ) 
                     => Configuration -> r -> AttemptT m a
makeSingleRequestAll cfg r = do
    AttemptT . liftIO $ getResult <$> withManager (\m -> makeDefaultRequestAll cfg m r)

-- | Given a Changeid returns the change info status for the corresponding 
--   request.
--
getChangeStatus :: (MonadIO m, Applicative m) => Configuration -> ChangeId -> AttemptT m ChangeInfoStatus
getChangeStatus cfg changeId = 
    ciStatus . gcrChangeInfo <$> (makeSingleRequest cfg $ getChange changeId)

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
getAllZones :: (MonadIO m, Applicative m) => Configuration -> AttemptT m HostedZones
getAllZones cfg = lhzrHostedZones <$> makeSingleRequestAll cfg listHostedZones

-- | Get a hosted zone by its 'HostedZoneId'.
--
getZoneById :: (MonadIO m, Applicative m) => Configuration -> HostedZoneId -> AttemptT m HostedZone
getZoneById cfg hzid = ghzrHostedZone <$> makeSingleRequest cfg (getHostedZone hzid)

-- | Get a hosted zone by its domain name.
--   
--   Results in an error if no hosted zone exists for the given domain name.
--
getZoneByName :: (MonadIO m, Applicative m) => Configuration -> Domain -> AttemptT m HostedZone
getZoneByName cfg z = maybe (fail err) return =<< (find ((z==) . hzName) <$> getAllZones cfg)
    where
    err = "Hosted zone not found: " ++ unpack (dText z)


-- | Returns the hosted zone id of the hosted zone for the given domain.
--
getZoneIdByName :: (MonadIO m, Applicative m) => Configuration -> Domain -> AttemptT m HostedZoneId
getZoneIdByName cfg hzName = hzId <$> getZoneByName cfg hzName

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
getResourceRecordSetsByHostedZoneName :: (MonadIO m, Applicative m) => Configuration -> Domain -> AttemptT m ResourceRecordSets
getResourceRecordSetsByHostedZoneName cfg zName = do
    hzid <- getZoneIdByName cfg zName
    lrrsrResourceRecordSets <$> makeSingleRequestAll cfg (listResourceRecordSets hzid)

-- | Lists all resource record sets in the hosted zone with the given hosted 
--   zone id.
--
getResourceRecordSets :: (MonadIO m, Applicative m) => Configuration -> HostedZoneId -> AttemptT m ResourceRecordSets
getResourceRecordSets cfg hzid = 
    lrrsrResourceRecordSets <$> makeSingleRequestAll cfg (listResourceRecordSets hzid)

-- | Lists all resource record sets in the given hosted zone for the given 
--   domain.
--
getResourceRecordSetsByDomain :: (MonadIO m, Applicative m) => Configuration -> HostedZoneId -> Domain -> AttemptT m ResourceRecordSets
getResourceRecordSetsByDomain cfg hzid domain = do
    filter ((== domain) . rrsName) <$> getResourceRecordSets cfg hzid

-- | Returns all resource records sets in the hosted zone with the given hosted
--   zone id for the given DNS record type.
--
getResourceRecordSetsByType :: (MonadIO m, Applicative m) => Configuration -> HostedZoneId -> RecordType -> AttemptT m ResourceRecordSets
getResourceRecordSetsByType cfg hzid dnsRecordType = 
    filter ((== dnsRecordType) . rrsType) <$> getResourceRecordSets cfg hzid

-- | Returns all resource records sets in the hosted zone with the given hosted
--   zone id for the given domain and the given DNS record type.
--
getResourceRecordSetsByDomainAndType :: (MonadIO m, Applicative m) => Configuration -> HostedZoneId -> Domain -> RecordType -> AttemptT m ResourceRecordSets
getResourceRecordSetsByDomainAndType cfg hzid domain dnsRecordType =
    let f ResourceRecordSet{..} = (rrsType == dnsRecordType) && (rrsName == domain)
    in  filter f <$> getResourceRecordSets cfg hzid

-- | Updates the resouce records of the given type for the given domain in the 
--   given hosted zone using the given mapping function.
--
--   Recall that the functions in this module are example usages of the 
--   Aws.Route53 module. In a production environment one would reuse the same 
--   connection manager and configuration for all involved requests.
--
modifyRecords :: (MonadIO m, Applicative m)
              =>Configuration
              -> HostedZoneId 
              -> Domain 
              -> RecordType 
              -> ([ResourceRecord] -> [ResourceRecord]) 
              -> AttemptT m [(ChangeResourceRecordSetsResponse, ChangeResourceRecordSetsResponse)]
modifyRecords cfg cid domain rtype f = runListT $ do
    rrs <- ListT $ getResourceRecordSetsByDomainAndType cfg cid domain rtype
    let rrs' = rrs { rrsRecords = f (rrsRecords rrs) }
    -- FIXME: what if we fail in the second call? Should we try to rollback?
    r1 <- lift . makeSingleRequest cfg $ ChangeResourceRecordSets cid Nothing [(DELETE, rrs)] 
    r2 <- lift . makeSingleRequest cfg $ ChangeResourceRecordSets cid Nothing [(CREATE, rrs')]
    return (r1, r2)

retry :: (MonadIO m, MonadPlus m) => Int -> Int -> m a -> m a
retry _ 1 req = req
retry pause num req | num < 0   = error $ "Illegal argument to retry. Expected positive Int, got " ++ (show num)
                    | otherwise = req `mplus` (wait >> retry pause (num-1) req)
    where
    wait = liftIO . threadDelay $ (pause * 1000000)


-- | Updates the A record for the given domain in the given zone to the given 
--   IP address (encoded as Text).
--
setARecordRetry :: (MonadIO m, Applicative m)
                => Int           -- ^ pause between retries (in seconds)
                -> Int           -- ^ number of retried attempts
                -> Configuration -- ^ Configuration
                -> HostedZoneId  -- ^ Zone ID
                -> Domain        -- ^ Domain
                -> Int           -- ^ TTL for the record
                -> IPv4          -- ^ The new value for the A record, an IPv4 address
                -> AttemptT m [ChangeResourceRecordSetsResponse]
setARecordRetry pause num cfg cid domain ttl ip = runListT $ del `mplus` ins
    where
    del = do
        rrs <- ListT . ret $ getResourceRecordSetsByDomainAndType cfg cid domain A
        lift . ret . makeSingleRequest cfg $ ChangeResourceRecordSets cid Nothing [(DELETE, rrs)]
    ins = do
        let rr = simpleResourceRecordSet domain A ttl (pack . show $ ip)
        lift . ret . makeSingleRequest cfg $ ChangeResourceRecordSets cid Nothing [(CREATE, rr)]
    ret = retry pause num

-- | Updates the A record for the given domain in the given zone to the given 
--   IP address (encoded as Text).
--
setARecord :: (MonadIO m, Applicative m)
           => Configuration
           -> HostedZoneId -- ^ Zone ID
           -> Domain       -- ^ Domain
           -> Int          -- ^ TTL for the record
           -> IPv4         -- ^ The new value for the A record, an IPv4 address
           -> AttemptT m [ChangeResourceRecordSetsResponse]
setARecord cfg cid domain ttl ip = setARecordRetry 0 1 cfg cid domain ttl ip

