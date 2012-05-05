{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-} 
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
  
module Aws.Route53.Core
( -- * Info
  Route53Info(..)
, route53EndpointUsClassic
, route53

  -- * Error
, Route53Error(..)

  -- * Metadata
, Route53Metadata(..)

  -- * Query
, route53SignQuery

  -- * Response
, route53ResponseConsumer
, route53CheckResponseType

  -- * Model

  -- ** DNS
, RecordType(..)
, typeToString

  -- ** Hosted Zone
, HostedZone (..)
, HostedZones
, Domain(..)
, HostedZoneId(..)

  -- ** Delegation Set
, DelegationSet(..)
, Nameserver
, Nameservers
, dsNameservers

  -- ** Resource Record Set
, REGION(..)
, ResourceRecordSets
, ResourceRecordSet(..)
, ResourceRecords
, ResourceRecord(..)
, AliasTarget(..)

  -- ** Change Info
, ChangeInfo(..)
, ChangeInfoStatus(..)
, ChangeId(..)

  -- * Parser Utilities
, Route53Parseable(..)
, Route53XmlSerializable(..)
, Route53Id(..)

  -- * HTTP Utilites
  -- | This functions extend 'Network.HTTP.Types'
, findHeader
, findHeaderValue
, headerRequestId
) where

import           Aws.Core
import           Data.IORef
import           Data.Monoid
import           Data.String
import           Data.Typeable
import           Control.Monad        (MonadPlus, mzero, mplus, liftM)
import           Data.List            (find)
import           Data.Maybe           (listToMaybe, fromJust)
import           Data.Text            (Text, unpack)
import           Data.Text.Encoding   (decodeUtf8)
import           Data.Time            (UTCTime)
import           Data.Time.Format     (parseTime)
import           System.Locale        (defaultTimeLocale)
import           Text.Hamlet.XML      (xml)
import           Text.XML             (elementAttributes)
import           Text.XML.Cursor      (($/), ($//), (&|), ($.//), laxElement)
import qualified Control.Exception    as C
import qualified Control.Failure      as F
import qualified Data.ByteString      as B
import qualified Data.Text            as T
import qualified Data.Text.Encoding   as T
import qualified Network.HTTP.Conduit as HTTP
import qualified Network.HTTP.Types   as HTTP
import qualified Text.XML             as XML
import qualified Text.XML.Cursor      as Cu

-- -------------------------------------------------------------------------- --
-- Info

data Route53Info = Route53Info 
    { route53Protocol :: Protocol
    , route53Endpoint :: B.ByteString
    , route53Port :: Int
    , route53ApiVersion :: B.ByteString
    , route53XmlNamespace :: T.Text

    } deriving (Show)

route53EndpointUsClassic :: B.ByteString
route53EndpointUsClassic = "route53.amazonaws.com"

route53ApiVersionRecent :: B.ByteString
route53ApiVersionRecent = "2012-02-29"

route53XmlNamespaceRecent :: Text
route53XmlNamespaceRecent = "https://route53.amazonaws.com/doc/" `T.append` T.decodeUtf8 route53ApiVersionRecent `T.append` "/"

route53 :: Route53Info
route53 = Route53Info 
    { route53Protocol = HTTPS
    , route53Endpoint = route53EndpointUsClassic
    , route53Port = defaultPort HTTPS
    , route53ApiVersion = route53ApiVersionRecent
    , route53XmlNamespace = route53XmlNamespaceRecent
    }

-- -------------------------------------------------------------------------- --
-- Error

-- TODO route53 documentation seem to indicate that there is also a type field in the error response body.
-- http://docs.amazonwebservices.com/Route53/latest/DeveloperGuide/ResponseHeader_RequestID.html

data Route53Error = Route53Error
      { route53StatusCode   :: HTTP.Status
      , route53ErrorCode    :: Text
      , route53ErrorMessage :: Text
      } deriving (Show, Typeable)

instance C.Exception Route53Error

-- -------------------------------------------------------------------------- --
-- Metadata

data Route53Metadata = Route53Metadata 
    { requestId :: Maybe T.Text
    } deriving (Show, Typeable)

instance Monoid Route53Metadata where
    mempty = Route53Metadata Nothing
    Route53Metadata r1 `mappend` Route53Metadata r2 = Route53Metadata (r1 `mplus` r2)

-- -------------------------------------------------------------------------- --
-- Query

route53SignQuery :: Method -> B.ByteString -> [(B.ByteString, B.ByteString)] -> Maybe XML.Element -> Route53Info -> SignatureData -> SignedQuery
route53SignQuery method resource query body Route53Info{..} sd
    = SignedQuery {
        sqMethod        = method
      , sqProtocol      = route53Protocol 
      , sqHost          = route53Endpoint
      , sqPort          = route53Port
      , sqPath          = route53ApiVersion `B.append` resource
      , sqQuery         = HTTP.simpleQueryToQuery query'
      , sqDate          = Just $ signatureTime sd
      , sqAuthorization = Nothing
      , sqContentType   = Nothing
      , sqContentMd5    = Nothing
      , sqAmzHeaders    = [("X-Amzn-Authorization", authorization)]
      , sqOtherHeaders  = []
      , sqBody          = renderBody `fmap` body
      , sqStringToSign  = stringToSign
      }
    where
      stringToSign  = fmtRfc822Time (signatureTime sd)
      credentials   = signatureCredentials sd
      accessKeyId   = accessKeyID credentials
      authorization = B.concat [ "AWS3-HTTPS AWSAccessKeyId="
                               , accessKeyId
                               , ", Algorithm=HmacSHA256, Signature="
                               , signature credentials HmacSHA256 stringToSign
                               ]
      query' = ("AWSAccessKeyId", accessKeyId) : query

      renderBody b = HTTP.RequestBodyLBS . XML.renderLBS XML.def $ XML.Document 
                     { XML.documentPrologue = XML.Prologue [] Nothing []
                     , XML.documentRoot = b { elementAttributes = addNamespace (elementAttributes b) }
                     , XML.documentEpilogue = []
                     }
      addNamespace attrs = maybe (("xmlns",route53XmlNamespace):attrs) (const attrs) $ lookup "xmlns" attrs
                           

-- -------------------------------------------------------------------------- --
-- Response

-- TODO: the documentation seems to indicate that in case of errors the requestId is returned in the body
--       Have a look at Ses/Response.hs how to parse the requestId element. We may try both (header and
--       body element) on each response and sum the results with `mplus` in the Maybe monad.
--       http://docs.amazonwebservices.com/Route53/latest/DeveloperGuide/ResponseHeader_RequestID.html

route53ResponseConsumer :: (Cu.Cursor -> Response Route53Metadata a)
                        -> IORef Route53Metadata
                        -> HTTPResponseConsumer a
route53ResponseConsumer inner metadataRef status headers =
    xmlCursorConsumer parse metadataRef status headers
    where
      parse cursor = do
        tellMetadata . Route53Metadata . fmap decodeUtf8 $ findHeaderValue headers headerRequestId
        case cursor $/ Cu.laxElement "Error" of
          []      -> inner cursor
          (err:_) -> fromError err

      fromError cursor = do
        errCode    <- force "Missing Error Code"    $ cursor $// elContent "Code"
        errMessage <- force "Missing Error Message" $ cursor $// elContent "Message"
        F.failure $ Route53Error status errCode errMessage


route53CheckResponseType :: F.Failure XmlException m => a -> Text -> Cu.Cursor -> m a
route53CheckResponseType a n c = do 
    _ <- force ("Expected response type " ++ unpack n) (Cu.laxElement n c)
    return a

-- ** Response types

-- TODO analyse the possible response types. I think there are common patterns.
-- Collect common code from the Commands here

-- -------------------------------------------------------------------------- --
-- Model

class Route53Id r where
  idQualifier :: r -> T.Text
  idText :: r -> T.Text
  
  asId :: T.Text -> r
  asId t = asId' . fromJust .T.stripPrefix (qualifiedIdTextPrefix (undefined::r)) $ t

  qualifiedIdTextPrefix :: r -> T.Text
  qualifiedIdTextPrefix r = "/" `T.append` idQualifier r `T.append` "/" 

  qualifiedIdText :: r -> T.Text
  qualifiedIdText r = qualifiedIdTextPrefix r `T.append` idText r

  -- | Helper for defining 'asId'. Constructs 'r' from a 'T.Text' assuming that
  --   the qualifier with already stripped from the argument.
  --
  --   Define either this or 'asId'. Usually defining 'asId'' is easier.
  asId' :: (T.Text -> r)
  asId' t = asId $ qualifiedIdTextPrefix (undefined::r) `T.append` t

--instance (Route53Id r) => IsString r where
--  fromString = HostedZoneId . fromJust . T.stripPrefix (idPrefix undefined) . T.pack

-- -------------------------------------------------------------------------- --
-- DNS

data RecordType = A | AAAA | NS | TXT | MX | CNAME | SOA | PTR | SRV | SPF | UNKNOWN Int 
                deriving (Eq, Show, Read)

typeToString :: RecordType -> String
typeToString = show

typeToText :: RecordType -> T.Text
typeToText = T.pack . typeToString

-- -------------------------------------------------------------------------- --
-- HostedZone

newtype HostedZoneId = HostedZoneId { hziText :: T.Text }
                        deriving (Show, IsString, Eq)

instance Route53Id HostedZoneId where
  idQualifier = const "hostedzone"
  idText = hziText
  --asId r = HostedZoneId . fromJust . T.stripPrefix (qualifiedIdTextPrefix (undefined::HostedZoneId)) $ r
  asId' = HostedZoneId

newtype Domain = Domain { dText :: T.Text }
                 deriving (Show, Eq)

instance IsString Domain where
  fromString = Domain . T.pack

type HostedZones = [HostedZone]

data HostedZone = HostedZone 
                  { hzId :: HostedZoneId
                  , hzName :: Domain
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
    zoneId <- force "Missing hostedZoneId element" $ c $/ elContent "Id" &| asId
    name <- force "Missing Name element" $ c $/ elContent "Name" &| Domain
    callerReference <- force "Missing CallerReference element" $ c $/ elContent "CallerReference"
    comment <- force "Missing Comment element" $ c $// elContent "Comment"
    resourceRecordSetCount <- forceM "Missing ResourceRecordCount" $ c $/ elCont "ResourceRecordSetCount" &| readInt
    return $ HostedZone zoneId name callerReference comment resourceRecordSetCount

instance Route53XmlSerializable HostedZone where

  toXml HostedZone{..} = XML.Element "HostedZone" [] [xml|
    <Id>#{idText hzId}
    <Name>#{dText hzName}
    <CallerReference>#{hzCallerReference}
    <Config>
      <Comment>#{hzComment}
    <ResourceRecordSetCount>#{intToText hzResourceRecordSetCount}
    |]

instance Route53XmlSerializable HostedZones where
  toXml hostedZones = XML.Element "HostedZones" [] $ (XML.NodeElement . toXml) `map` hostedZones

-- -------------------------------------------------------------------------- --
-- Delegation Set

type Nameservers = [Nameserver]

type Nameserver = Domain

data DelegationSet = DelegationSet { dsNameserver1 :: Domain 
                                   , dsNameserver2 :: Domain
                                   , dsNameserver3 :: Domain
                                   , dsNameserver4 :: Domain
                                   } deriving (Show)

dsNameservers :: DelegationSet -> [Domain]
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
    force "Missing Nameserver element" $ cursor $.// elContent "Nameserver" &| Domain

-- -------------------------------------------------------------------------- --
-- ResourceRecordSet

data REGION = ApNorthEast1
            | ApSouthEast2
            | EuWest1
            | SaEast1
            | UsEast1
            | UsWest1
            | UsWest2
            | UnknownRegion
            deriving (Eq)

instance Show REGION where
  show ApNorthEast1  = "ap-north-east-1"
  show ApSouthEast2  = "ap-South-east-2"
  show EuWest1       = "eu-west-1"
  show SaEast1       = "sa-east-1"
  show UsEast1       = "us-east-1"
  show UsWest1       = "us-west-1"
  show UsWest2       = "us-west-2"
  show UnknownRegion = "unknown"

regionToText :: REGION -> T.Text
regionToText = T.pack . show

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
                         deriving (Show, Eq)

data AliasTarget = AliasTarget { atHostedZoneId :: HostedZoneId
                               , atDNSName :: Domain
                               } deriving (Show)

-- TODO make this complete from the spec. Do not just use the exmpales!
-- We may e.g. have different type for alias resource record sets
data ResourceRecordSet = ResourceRecordSet { rrsName :: Domain
                                           , rrsType :: RecordType
                                           , rrsAliasTarget :: Maybe AliasTarget
                                           , rrsSetIdentifier :: Maybe T.Text
                                           , rrsWeight :: Maybe Int
                                           , rrsRegion :: Maybe REGION
                                           , rrsTTL  :: Maybe Int
                                           , rrsRecords :: ResourceRecords
                                           } deriving (Show)
                                           
type ResourceRecordSets = [ResourceRecordSet]

instance Route53XmlSerializable ResourceRecordSet where

  toXml ResourceRecordSet{..} = XML.Element "ResourceRecordSet" [] [xml|
    <Name>#{dText rrsName}
    <Type>#{typeToText rrsType}
    $maybe a <- rrsAliasTarget
      <AliasTarget>
        ^{[XML.NodeElement (toXml a)]}
    $maybe i <- rrsSetIdentifier 
      <SetIdentifier>#{i}
    $maybe w <- rrsWeight
      <Weight>#{intToText w}
    $maybe r <- rrsRegion
      <Region>#{regionToText r}
    $maybe t <- rrsTTL
      <TTL>#{intToText t}
    <ResourceRecords>
      $forall record <- rrsRecords
        ^{[XML.NodeElement (toXml record)]}
    |]

instance Route53XmlSerializable ResourceRecord where
  
  toXml ResourceRecord{..} = XML.Element "ResourceRecord" [] [xml|  <Value>#{value} |]

instance Route53XmlSerializable AliasTarget where
  
  toXml AliasTarget{..} = XML.Element "AliasTarget" [] [xml|
    <HostedZoneId>#{idText atHostedZoneId}
    <DNSName>#{dText atDNSName}
    |]

--instance Route53XmlSerializable HostedZones where
--  toXml hostedZones = XML.Element "HostedZones" [] $ (XML.NodeElement . toXml) `map` hostedZones

instance Route53Parseable ResourceRecordSets where
  r53Parse cursor = do
    c <- force "Missing ResourceRecordSets element" $ cursor $.// laxElement "ResourceRecordSets"
    sequence $ c $/ laxElement "ResourceRecordSet" &| r53Parse

instance Route53Parseable ResourceRecordSet where
  r53Parse cursor = do
    c <- force "Missing ResourceRecordSet element" $ cursor $.// laxElement "ResourceRecordSet"
    name <- force "Missing name element" $ c $/ elContent "Name" &| Domain
    dnsType <- force "Missing type element" $ c $/ elCont "Type" &| read
    ttl <- listToMaybe `liftM` (sequence $ c $/ elCont "TTL" &| readInt)
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
    zoneId <- force "Missing HostedZoneId element" $ c $/ elContent "HostedZoneId" &| asId
    dnsName <- force "Missing DNSName element" $ c $/ elContent "DNSName" &| Domain
    return $ AliasTarget zoneId dnsName


instance Route53Parseable ResourceRecords where
  r53Parse cursor = do
    c <- force "Missing ResourceRecords element" $ cursor $.// laxElement "ResourceRecords"
    sequence $ c $/ laxElement "ResourceRecord" &| r53Parse

instance Route53Parseable ResourceRecord where
  r53Parse cursor = do
    c <- force "Missing ResourceRecord element" $ cursor $.// laxElement "ResourceRecord"
    force "Missing Value element" $ c $/ elContent "Value" &| ResourceRecord

-- -------------------------------------------------------------------------- --
-- Change Info

data ChangeInfoStatus = PENDING | INSYNC
                        deriving (Show, Read)

newtype ChangeId = ChangeId { changeIdText :: T.Text }
                   deriving (Show, Eq)

instance Route53Id ChangeId where
  idQualifier = const "change"
  idText = changeIdText
  asId' = ChangeId

data ChangeInfo = ChangeInfo { ciId :: ChangeId
                             , ciStatus :: ChangeInfoStatus
                             , ciSubmittedAt :: UTCTime
                             } deriving (Show)

instance Route53Parseable ChangeInfo where
  r53Parse cursor = do
    c <- force "Missing ChangeInfo element" $ cursor $.// laxElement "ChangeInfo"
    ciId <- force "Missing Id element" $ c $/ elContent "Id" &| asId
    status <- force "Missing Status element" $ c $/ elCont "Status" &| read
    submittedAt <- force "Missing SubmittedAt element" $ c $/ elCont "SubmittedAt" &| utcTime
    return $ ChangeInfo ciId status submittedAt
    where
    utcTime str = fromJust $ parseTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S%Q%Z" str

-- -------------------------------------------------------------------------- --
-- Parser and Serialization Utilities

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

class Route53XmlSerializable r where
  toXml :: r -> XML.Element 

intToText :: Int -> T.Text
intToText = T.pack . show

-- -------------------------------------------------------------------------- --
-- Utility methods that extend the functionality of 'Network.HTTP.Types' 

headerRequestId :: HTTP.Ascii -> HTTP.Header
headerRequestId = (,) "x-amzn-requestid"

findHeader :: [HTTP.Header] -> (HTTP.Ascii -> HTTP.Header) -> Maybe HTTP.Header
findHeader headers header = find (\h@(_,v) -> h == header v) headers

findHeaderValue :: [HTTP.Header] -> (HTTP.Ascii -> HTTP.Header) -> Maybe HTTP.Ascii
findHeaderValue headers = fmap snd . findHeader headers

