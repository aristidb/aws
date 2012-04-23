{-# LANGUAGE 
    OverloadedStrings
  , FlexibleContexts
  , RecordWildCards
  , TypeSynonymInstances 
  #-}
module Aws.Route53.Model
( HostedZone (..)
, HostedZones
, DelegationSet(..)
, Nameserver
, Nameservers
, dsNameservers
, ResourceRecordSets
, ResourceRecordSet(..)
, ResourceRecords
, ResourceRecord(..)
, AliasTarget(..)
, findHeader
, findHeaderValue
, headerRequestId
, Route53Parseable(..)
, typeToString
)

where

import           Control.Monad      (MonadPlus, mzero, mplus, liftM)
import           Aws.Xml
import           Text.XML.Cursor    (($/), ($//), (&|), ($.//), laxElement)
import           Data.Text.Encoding (encodeUtf8)
import           Data.List          (find)
import           Data.Maybe         (listToMaybe)
import qualified Control.Failure    as F
import qualified Text.XML.Cursor    as Cu
import qualified Data.Text          as T
import qualified Data.Text.Encoding as T
import qualified Network.DNS.Types  as DNS
import qualified Network.HTTP.Types as HTTP

-- * HostedZone

type HostedZones = [HostedZone]

-- | A hosted zone is
data HostedZone = HostedZone 
                  { hzId :: T.Text
                  , hzName :: DNS.Domain
                  , hzCallerReference :: T.Text
                  , hzComment :: T.Text
                  , hzResourceRecordSetCount :: Int
                  } deriving (Show)

instance Route53Parseable HostedZones where
  r53Parse cursor = do
    c <- force "Missing HostedZones element" $ cursor $.// laxElement "HostedZones"
    sequence $ c $/ laxElement "HostedZone" &| r53Parse

instance Route53Parseable HostedZone where
  r53Parse cursor = do
    c <- force "Missing HostedZone element" $ cursor $.// laxElement "HostedZone"
    zoneId <- force "Missing hostedZoneId element" $ c $/ elContent "Id"
    name <- force "Missing Name element" $ c $/ elContent "Name" &| encodeUtf8
    callerReference <- force "Missing CallerReference element" $ c $/ elContent "CallerReference"
    comment <- force "Missing Comment element" $ c $// elContent "Comment"
    resourceRecordSetCount <- forceM "Missing ResourceRecordCount" $ c $/ elCont "ResourceRecordSetCount" &| readInt
    return $ HostedZone zoneId name callerReference comment resourceRecordSetCount

-- * Delegation Set

-- | Currently only internally used for composing parsers
type Nameservers = [Nameserver]

-- | Currently only internally used for composing parsers
type Nameserver = DNS.Domain

data DelegationSet = DelegationSet { dsNameserver1 :: DNS.Domain 
                                   , dsNameserver2 :: DNS.Domain
                                   , dsNameserver3 :: DNS.Domain
                                   , dsNameserver4 :: DNS.Domain
                                   } deriving (Show)

dsNameservers :: DelegationSet -> [DNS.Domain]
dsNameservers DelegationSet{..} = [dsNameserver1, dsNameserver2, dsNameserver3, dsNameserver4]

instance Route53Parseable DelegationSet where
  r53Parse cursor = do
    c <- force "Missing DelegationSet element" $ cursor $.// laxElement "DelegationSet"
    [ns1, ns2, ns3, ns4] <- forceTake 4 "Expected four nameservers in DelegationSet" =<< r53Parse c
    return $ DelegationSet ns1 ns2 ns3 ns4

instance Route53Parseable Nameservers where
  r53Parse cursor = do
    c <- force "Missing Nameservers element" $ cursor $.// laxElement "Nameservers"
    sequence $ c $/ laxElement "Nameserver" &| r53Parse

instance Route53Parseable Nameserver where
  r53Parse cursor = 
    force "Missing Nameserver element" $ cursor $.// elContent "Nameserver" &| T.encodeUtf8

-- * RsourceRecordSet

data REGION = ApNorthEast1
            | ApSouthEast2
            | EuWest1
            | SaEast1
            | UsEast1
            | UsWest1
            | UsWest2
            | UnknownRegion

instance Show REGION where
  show ApNorthEast1  = "ap-north-east-1"
  show ApSouthEast2  = "ap-South-east-2"
  show EuWest1       = "eu-west-1"
  show SaEast1       = "sa-east-1"
  show UsEast1       = "us-east-1"
  show UsWest1       = "us-west-1"
  show UsWest2       = "us-west-2"
  show UnknownRegion = "unknown"

regionFromString :: String -> REGION
regionFromString "ap-north-east-1" = ApNorthEast1
regionFromString "ap-South-east-2" = ApSouthEast2
regionFromString "eu-west-1"       = EuWest1
regionFromString "sa-east-1"       = SaEast1
regionFromString "us-east-1"       = UsEast1
regionFromString "us-west-1"       = UsWest1
regionFromString "us-west-2"       = UsWest2
regionFromString _                 = UnknownRegion

type ResourceRecords = [ResourceRecord]

newtype ResourceRecord = ResourceRecord { value :: T.Text }
                         deriving (Show)

data AliasTarget = AliasTarget { atHostedZoneId :: T.Text
                               , atDNSName :: DNS.Domain
                               } deriving (Show)

-- TODO make this complete from the spec. Do not just use the exmpales!
data ResourceRecordSet = ResourceRecordSet { rrsName :: DNS.Domain
                                           , rrsType :: DNS.TYPE
                                           , rrsAliasTarget :: Maybe AliasTarget
                                           , rrsSetIdentifier :: Maybe T.Text
                                           , rrsWeight :: Maybe Int
                                           , rssRegion :: Maybe REGION
                                           , rrsTTL  :: Int
                                           , rrsRecords :: ResourceRecords
                                           } deriving (Show)
                                           
type ResourceRecordSets = [ResourceRecordSet]

instance Route53Parseable ResourceRecordSets where
  r53Parse cursor = do
    c <- force "Missing ResourceRecordSets element" $ cursor $.// laxElement "ResourceRecordSets"
    sequence $ c $/ laxElement "ResourceRecordSet" &| r53Parse

instance Route53Parseable ResourceRecordSet where
  r53Parse cursor = do
    c <- force "Missing ResourceRecordSet element" $ cursor $.// laxElement "ResourceRecordSet"
    name <- force "Missing name element" $ c $/ elContent "Name" &| encodeUtf8
    dnsType <- force "Missing type element" $ c $/ elCont "Type" &| DNS.toType 
    ttl <- forceM "Missing TTL element" $ c $/ elCont "TTL" &| readInt
    alias <- listToMaybe `liftM` (sequence $ c $/ laxElement "AliasTarget" &| r53Parse)
    let setIdentifier = listToMaybe $ c $/ elContent "SetIdentifier"
    weight <- listToMaybe `liftM` (sequence $ c $/ elCont "Weight" &| readInt)
    let region = listToMaybe $ c $/ elCont "Region" &| regionFromString
    resourceRecords <- r53Parse c
    return $ ResourceRecordSet name dnsType alias setIdentifier weight region ttl resourceRecords

-- TODO is there any constraint on the number of records?
-- TODO check constraints on type

instance Route53Parseable AliasTarget where
  r53Parse cursor = do
    c <- force "Missing AliasTarget element" $ cursor $.// laxElement "AliasTarget"
    zoneId <- force "Missing HostedZoneId element" $ c $/ elContent "HostedZoneId"
    dnsName <- force "Missing DNSName element" $ c $/ elContent "DNSName" &| encodeUtf8
    return $ AliasTarget zoneId dnsName


instance Route53Parseable ResourceRecords where
  r53Parse cursor = do
    c <- force "Missing ResourceRecords element" $ cursor $.// laxElement "ResourceRecords"
    sequence $ c $/ laxElement "ResourceRecord" &| r53Parse

instance Route53Parseable ResourceRecord where
  r53Parse cursor = do
    c <- force "Missing ResourceRecord element" $ cursor $.// laxElement "ResourceRecord"
    force "Missing Value element" $ c $/ elContent "Value" &| ResourceRecord

-- * Parser Utilities

-- | A class for Route53 XML response parsers
--
--   TODO Move these utilties to another module, for instance 'Aws.Route53.ParserUtils'
--
--   Parsers work with the following scheme:
--
--   * A parsers target either a single node or a set of ndoes.
--
--   * A parser that targets a single node will parse the first matching node that it finds.
--   
--   * A cursor with a node that is the target node it self or a parent of the target nodes.
--   
--   * The parser fails if it targets a single node and that nodes does not exist.
--   
--   * For multiple target nodes the parser may return the empty list.
--
--  TODO there is a lot of Boilerplat here. With only little overhead serializatin and deserialization
--  could be derived from the instance declaration. Maybe some DLS would be a goold solution

class Route53Parseable r where

  r53Parse :: F.Failure XmlException m => Cu.Cursor -> m r

-- | Takes the first @n@ elements from a List and injects them into a 'MonadPlus'. 
--   Causes a failure in the 'Control.Failure' Monad if there are not enough elements 
--   in the List. 
forceTake :: (F.Failure XmlException f, MonadPlus m) => Int -> String -> [a] -> f (m a)
forceTake 0 _ _ = return mzero
forceTake _ e [] = force e []
forceTake n e l = do 
  h <- force e l
  t <- forceTake (n-1) e (tail l)
  return $ return h  `mplus` t

-- * Utility methods that extend the functionality of 'Network.HTTP.Types' and 'Network.DNS.Types'

headerRequestId :: HTTP.Ascii -> HTTP.Header
headerRequestId = (,) "x-amzn-requestid"

findHeader :: [HTTP.Header] -> (HTTP.Ascii -> HTTP.Header) -> Maybe HTTP.Header
findHeader headers header = find (\h@(_,v) -> h == header v) headers

findHeaderValue :: [HTTP.Header] -> (HTTP.Ascii -> HTTP.Header) -> Maybe HTTP.Ascii
findHeaderValue headers = fmap snd . findHeader headers

typeToString :: DNS.TYPE -> String
typeToString = show
