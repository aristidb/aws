{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}

module Route53.Samples where

import qualified Data.Text as T
import qualified Aws as AWS
import           Aws.Route53
import           Data.Attempt
import           Control.Applicative ((<$>))
import           Network.HTTP.Conduit
import           Control.Monad.IO.Class (liftIO)
import           Data.List (find)
import           Data.Maybe (fromJust)

-- -------------------------------------------------------------------------- --
-- Request Utils

-- | A class for transactions with batched responses. Provides methods
--   for iterating and concatenating all responses.
--
--   Minimal complete implementation: 'merge' and 'nextRequest'.
class (AWS.Transaction request response) => Batched request response  where
  merge :: response -> response -> response
  nextRequest :: request -> response -> Maybe request

  requestAll :: (request -> IO response) -> request -> IO response
  requestAll mkRequest request = do
    response <- mkRequest request
    case nextRequest request response of
      Nothing -> return response
      Just r -> merge response <$> requestAll mkRequest r


-- | Given a configuration, a manager, and request the function executes the request
--   and extracts the enclosed response body and returns it within the IO monad.
--
--   Executes 'getResult' in the IO monad and will thus cause an error if the 
--   request is not successful.
makeRequest :: (AWS.Transaction request response, Show request) 
            => Route53Configuration -> Manager -> request -> IO response
makeRequest cfg manager request = AWS.aws cfg manager request >>= getResult

-- | Executes the given request using the default configuration and a fresh connection manager.
--   Extracts the enclosed response body and returns it within the IO monad.
--
--   Will result in an error in if request is not successful
makeSimpleRequest :: (AWS.Transaction request response, Show request) 
            => request -> IO response
makeSimpleRequest r = do
  cfg <- AWS.defaultConfiguration
  withManager $ \m -> do
    liftIO $ makeRequest cfg m r

-- | Given a Changeid returns the change info status for the corresponding request
getChangeStatus :: ChangeId -> IO ChangeInfoStatus
getChangeStatus changeId = ciStatus . gcrChangeInfo <$> (makeSimpleRequest $ getChange changeId)

-- | Extracts the result from a response within an FromAttempt Monad (IO, [], Maybe, etc)
getResult :: (FromAttempt error) => AWS.Response meta r -> error r
getResult (AWS.Response _ r) = fromAttempt r

-- | Extracts the ChangeId from a response using the given function to extract the ChangeInfo from the response
getChangeId :: Functor f => (a -> ChangeInfo) -> f a -> f ChangeId
getChangeId changeInfoExtractor response = ciId . changeInfoExtractor <$> response

-- | Example usage of getChangeId
getChangeResourceRecordSetsResponseChangeId :: Functor f => f ChangeResourceRecordSetsResponse -> f ChangeId
getChangeResourceRecordSetsResponseChangeId response = getChangeId crrsrChangeInfo response

-- TODO implement wait for INSYNC

-- -------------------------------------------------------------------------- --
-- Hosted Zones

instance Batched ListHostedZones ListHostedZonesResponse where
  
  a `merge` b = ListHostedZonesResponse { lhzrHostedZones = lhzrHostedZones a ++ lhzrHostedZones b
                                        , lhzrNextToken = lhzrNextToken b
                                        }

  nextRequest _ ListHostedZonesResponse{..} = maybe Nothing (\x -> Just $ ListHostedZones Nothing (Just x)) lhzrNextToken

-- | Get all hosted zones of the user.
getAllZones :: IO HostedZones
getAllZones = do
  cfg <- AWS.defaultConfiguration
  withManager $ \m -> do
    ListHostedZonesResponse zones _ <- liftIO $ requestAll (\r -> makeRequest cfg m r) listHostedZones
    return zones

-- | Get a hosted zone by its 'HostedZoneId'
getZoneById :: HostedZoneId -> IO HostedZone
getZoneById hzid = ghzrHostedZone <$> makeSimpleRequest (getHostedZone hzid)

-- | Get a hosted zone by its domain name.
--   
--   Results in an error if no hosted zone exists for the given domain name.
getZoneByName :: Domain -> IO HostedZone
getZoneByName z = fromJust . find ((z==) . hzName) <$> getAllZones

-- | Returns the hosted zone id of the hosted zone for the given domain
getZoneIdByName :: Domain -> IO HostedZoneId
getZoneIdByName hzName = hzId <$> getZoneByName hzName

-- -------------------------------------------------------------------------- --
-- Resource Records Sets

-- | Simplified construction for a ResourceRecordSet
simpleResourceRecordSet :: Domain -> RecordType -> Int -> T.Text -> ResourceRecordSet
simpleResourceRecordSet domain rtype ttl value = ResourceRecordSet domain rtype Nothing Nothing Nothing Nothing (Just ttl) [(ResourceRecord value)]

instance Batched ListResourceRecordSets ListResourceRecordSetsResponse where
  a `merge` b = ListResourceRecordSetsResponse
                { lrrsrResourceRecordSets = lrrsrResourceRecordSets a ++ lrrsrResourceRecordSets b
                , lrrsrIsTruncated = lrrsrIsTruncated b
                , lrrsrNextRecordName = lrrsrNextRecordName b
                , lrrsrNextRecordType = lrrsrNextRecordType b
                , lrrsrNextRecordIdentifier = lrrsrNextRecordIdentifier b
                , lrrsrMaxItems = lrrsrMaxItems b
                }
  nextRequest ListResourceRecordSets{..} ListResourceRecordSetsResponse{..} = 
    if lrrsrIsTruncated
      then Just $ ListResourceRecordSets lrrsHostedZoneId lrrsrNextRecordName lrrsrNextRecordType lrrsrNextRecordIdentifier lrrsrMaxItems
      else Nothing

-- | Returns the resource record sets in the hosted zone with the given domain name
--
--   Note the 'zName' is the domain name of the hosted zone itself.
getResourceRecordSetsByHostedZoneName :: Domain -> IO ResourceRecordSets
getResourceRecordSetsByHostedZoneName zName = do
  cfg <- AWS.defaultConfiguration
  hzid <- getZoneIdByName zName
  withManager $ \m -> do
    ListResourceRecordSetsResponse rs _ _ _ _ _ <- liftIO $ requestAll (\r -> makeRequest cfg m r) (listResourceRecordSets hzid)
    return rs

-- | Lists all resource record sets in the hosted zone with the given hosted zone id.
getResourceRecordSets :: HostedZoneId -> IO ResourceRecordSets
getResourceRecordSets hzid = do
  cfg <- AWS.defaultConfiguration
  withManager $ \m -> do
    ListResourceRecordSetsResponse rs _ _ _ _ _ <- liftIO $ requestAll (\r -> makeRequest cfg m r) (listResourceRecordSets hzid)
    return rs

-- | Lists all resource record sets in the given hosted zone for the given domain.
getResourceRecordSetsByDomain :: HostedZoneId -> Domain -> IO ResourceRecordSets
getResourceRecordSetsByDomain hzid domain = do
  cfg <- AWS.defaultConfiguration
  withManager $ \m -> do
    ListResourceRecordSetsResponse rs _ _ _ _ _ <- liftIO $ requestAll (\r -> makeRequest cfg m r) ((listResourceRecordSets hzid){ lrrsName = Just domain})
    return rs

-- | Returns all resource records sets in the hosted zone with the given hosted zone id for the given DNS record type.
getResourceRecordSetsByType :: HostedZoneId -> RecordType -> IO ResourceRecordSets
getResourceRecordSetsByType hzid dnsRecordType = filter ((== dnsRecordType) . rrsType) <$> getResourceRecordSets hzid

-- | Returns the resource record set of the given type for the given domain in the given hosted zone.
getResourceRecords :: HostedZoneId -> Domain -> RecordType -> IO ResourceRecordSet
getResourceRecords cid domain rtype = head . lrrsrResourceRecordSets <$> (makeSimpleRequest $ ListResourceRecordSets cid (Just domain) (Just rtype) Nothing (Just 1))

-- | Updates the resouce records of the given type for the given domain in the given hosted zone using the given mapping function.
--
--   Recall that the functions in this module are example usages of the Aws.Route53 module. In a production
--   environment one would reuse the same connection manager and configuration for all involved requests.
updateRecords :: HostedZoneId -> Domain -> RecordType -> ([ResourceRecord] -> [ResourceRecord]) -> IO (ChangeResourceRecordSetsResponse, ChangeResourceRecordSetsResponse)
updateRecords cid domain rtype f = do
  -- Fixme fail more gracefully
  rrs <- getResourceRecords cid domain rtype
  let rrs' = rrs { rrsRecords = f (rrsRecords rrs) }
  -- Handle errors gracefully. What if we fail in the middle?
  r1 <- makeSimpleRequest $ ChangeResourceRecordSets cid Nothing [(DELETE, rrs)]
  r2 <- makeSimpleRequest $ ChangeResourceRecordSets cid Nothing [(CREATE, rrs')]
  return (r1, r2)

-- | Updates the A record for the given domain in the given zone to the given IP address (encoded as Text)
updateARecord :: HostedZoneId -> Domain -> T.Text -> IO (ChangeResourceRecordSetsResponse, ChangeResourceRecordSetsResponse)
updateARecord cid domain newIP = updateRecords cid domain A (const [ResourceRecord newIP])


