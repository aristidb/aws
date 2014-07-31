{-# LANGUAGE DeriveDataTypeable        #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE UndecidableInstances      #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Aws.DynamoDb.Core
-- Copyright   :  Soostone Inc, Chris Allen
-- License     :  BSD3
--
-- Maintainer  :  Ozgun Ataman, Chris Allen
-- Stability   :  experimental
--
-- Shared types and utilities for DyanmoDb functionality.
----------------------------------------------------------------------------

module Aws.DynamoDb.Core
    (
    -- * Configuration and Regions
      Region (..)
    , ddbUsEast1
    , ddbUsWest1
    , ddbUsWest2
    , ddbEuWest1
    , ddbApNe1
    , ddbApSe1
    , ddbApSe2
    , ddbSaEast1
    , DdbConfiguration (..)

    -- * DynamoDB values
    , DValue (..)

    -- * Converting to/from 'DValue'
    , DynVal(..)
    , toValue, fromValue
    , Bin (..)

    -- * Defining new 'DynVal' instances
    , DynData(..)
    , DynBinary(..), DynNumber(..), DynString(..)

    -- * Working with key/value pairs
    , Attribute (..)
    , attrTuple
    , attr
    , attrAs
    , text, int, double
    , PrimaryKey (..)
    , hk
    , hrk

    -- * Working with objects (attribute collections)
    , Item
    , item

    -- * Common types used by operations
    , Expects (..)
    , expectsJson
    , Condition (..)
    , CondOp (..)
    , Expect (..)
    , ConsumedCapacity (..)
    , ReturnConsumption (..)
    , ItemCollectionMetrics (..)
    , ReturnItemCollectionMetrics (..)
    , UpdateReturn (..)

    -- * Responses & Errors
    , DdbResponse (..)
    , DdbErrCode (..)
    , DdbError (..)

    -- * Internal Helpers
    , ddbSignQuery
    , AmazonError (..)
    , ddbResponseConsumer
    , ddbHttp
    , ddbHttps

    ) where


-------------------------------------------------------------------------------
import           Control.Applicative
import qualified Control.Exception            as C
import           Control.Monad
import           Control.Monad.Trans
import           Control.Monad.Trans.Resource (throwM)
import           Crypto.Hash
import           Data.Aeson
import qualified Data.Aeson                   as A
import           Data.Aeson.Types             (Pair, parseEither)
import           Data.Byteable
import qualified Data.ByteString.Base16       as Base16
import qualified Data.ByteString.Base64       as Base64
import qualified Data.ByteString.Char8        as B
import           Data.Conduit
import           Data.Conduit.Attoparsec      (sinkParser)
import           Data.Default
import qualified Data.HashMap.Strict          as HM
import           Data.Int
import           Data.IORef
import qualified Data.Map                     as M
import           Data.Maybe
import           Data.Monoid
import           Data.Proxy
import           Data.Scientific
import qualified Data.Serialize               as Ser
import qualified Data.Set                     as S
import           Data.String
import qualified Data.Text                    as T
import qualified Data.Text.Encoding           as T
import           Data.Time
import           Data.Typeable
import           Data.Word
import qualified Network.HTTP.Conduit         as HTTP
import qualified Network.HTTP.Types           as HTTP
import           Safe
-------------------------------------------------------------------------------
import           Aws.Core
-------------------------------------------------------------------------------



-------------------------------------------------------------------------------
-- | Numeric values stored in DynamoDb. Only used in defining new
-- 'DynVal' instances.
newtype DynNumber = DynNumber { unDynNumber :: Scientific }
    deriving (Eq,Show,Read,Ord,Typeable)


-------------------------------------------------------------------------------
-- | String values stored in DynamoDb. Only used in defining new
-- 'DynVal' instances.
newtype DynString = DynString { unDynString :: T.Text }
    deriving (Eq,Show,Read,Ord,Typeable)


-------------------------------------------------------------------------------
-- | Binary values stored in DynamoDb. Only used in defining new
-- 'DynVal' instances.
newtype DynBinary = DynBinary { unDynBinary :: B.ByteString }
    deriving (Eq,Show,Read,Ord,Typeable)


-------------------------------------------------------------------------------
-- | An internally used closed typeclass for values that have direct
-- DynamoDb representations. Based on AWS API, this is basically
-- numbers, strings and binary blobs.
--
-- This is here so that any 'DynVal' haskell value can automatically
-- be lifted to a list or a 'Set' without any instance code
-- duplication.
--
-- Do not try to create your own instances.
class Ord a => DynData a where
    fromData :: a -> DValue
    toData :: DValue -> Maybe a

instance DynData DynNumber where
    fromData (DynNumber i) = DNum i
    toData (DNum i) = Just $ DynNumber i
    toData _ = Nothing

instance DynData (S.Set DynNumber) where
    fromData set = DNumSet (S.map unDynNumber set)
    toData (DNumSet i) = Just $ S.map DynNumber i
    toData _ = Nothing

instance DynData DynString where
    fromData (DynString i) = DString i
    toData (DString i) = Just $ DynString i
    toData _ = Nothing

instance DynData (S.Set DynString) where
    fromData set = DStringSet (S.map unDynString set)
    toData (DStringSet i) = Just $ S.map DynString i
    toData _ = Nothing

instance DynData DynBinary where
    fromData (DynBinary i) = DBinary i
    toData (DBinary i) = Just $ DynBinary i
    toData _ = Nothing

instance DynData (S.Set DynBinary) where
    fromData set = DBinSet (S.map unDynBinary set)
    toData (DBinSet i) = Just $ S.map DynBinary i
    toData _ = Nothing

instance DynData DValue where
    fromData = id
    toData = Just


-------------------------------------------------------------------------------
-- | Class of Haskell types that can be represented as DynamoDb values.
--
-- This is the conversion layer; instantiate this class for your own
-- types and then use the 'toValue' and 'fromValue' combinators to
-- convert in application code.
--
-- Each Haskell type instantiated with this class will map to a
-- DynamoDb-supported type that most naturally represents it.
class DynData (DynRep a) => DynVal a where

    -- | Which of the 'DynData' instances does this data type directly
    -- map to?
    type DynRep a

    -- | Convert to representation
    toRep :: a -> DynRep a

    -- | Convert from representation
    fromRep :: DynRep a -> Maybe a


-------------------------------------------------------------------------------
-- | Any singular 'DynVal' can be upgraded to a list.
instance (DynData (DynRep [a]), DynVal a) => DynVal [a] where
    type DynRep [a] = S.Set (DynRep a)
    fromRep set = mapM fromRep $ S.toList set
    toRep as = S.fromList $ map toRep as


-------------------------------------------------------------------------------
-- | Any singular 'DynVal' can be upgraded to a 'Set'.
instance (DynData (DynRep [a]), DynVal a, Ord a) => DynVal (S.Set a) where
    type DynRep (S.Set a) = S.Set (DynRep a)
    fromRep set = fmap S.fromList . mapM fromRep $ S.toList set
    toRep as = S.map toRep as


instance DynVal DValue where
    type DynRep DValue = DValue
    fromRep = Just
    toRep   = id


instance DynVal Int where
    type DynRep Int = DynNumber
    fromRep (DynNumber i) = toIntegral i
    toRep i = DynNumber (fromIntegral i)


instance DynVal Int8 where
    type DynRep Int8 = DynNumber
    fromRep (DynNumber i) = toIntegral i
    toRep i = DynNumber (fromIntegral i)


instance DynVal Int16 where
    type DynRep Int16 = DynNumber
    fromRep (DynNumber i) = toIntegral i
    toRep i = DynNumber (fromIntegral i)


instance DynVal Int32 where
    type DynRep Int32 = DynNumber
    fromRep (DynNumber i) = toIntegral i
    toRep i = DynNumber (fromIntegral i)


instance DynVal Int64 where
    type DynRep Int64 = DynNumber
    fromRep (DynNumber i) = toIntegral i
    toRep i = DynNumber (fromIntegral i)


instance DynVal Word8 where
    type DynRep Word8 = DynNumber
    fromRep (DynNumber i) = toIntegral i
    toRep i = DynNumber (fromIntegral i)


instance DynVal Word16 where
    type DynRep Word16 = DynNumber
    fromRep (DynNumber i) = toIntegral i
    toRep i = DynNumber (fromIntegral i)


instance DynVal Word32 where
    type DynRep Word32 = DynNumber
    fromRep (DynNumber i) = toIntegral i
    toRep i = DynNumber (fromIntegral i)


instance DynVal Word64 where
    type DynRep Word64 = DynNumber
    fromRep (DynNumber i) = toIntegral i
    toRep i = DynNumber (fromIntegral i)


instance DynVal Integer where
    type DynRep Integer = DynNumber
    fromRep (DynNumber i) = toIntegral i
    toRep i = DynNumber (fromIntegral i)


instance DynVal T.Text where
    type DynRep T.Text = DynString
    fromRep (DynString i) = Just i
    toRep i = DynString i


instance DynVal B.ByteString where
    type DynRep B.ByteString = DynString
    fromRep (DynString i) = Just $ T.encodeUtf8 i
    toRep i = DynString (T.decodeUtf8 i)


instance DynVal Double where
    type DynRep Double = DynNumber
    fromRep (DynNumber i) = Just $ toRealFloat i
    toRep i = DynNumber (fromFloatDigits i)


instance DynVal Day where
    type DynRep Day = DynNumber
    fromRep (DynNumber i) = ModifiedJulianDay <$> (toIntegral i)
    toRep (ModifiedJulianDay i) = DynNumber (fromIntegral i)


-- | Encoded as 0 and 1.
instance DynVal Bool where
    type DynRep Bool = DynNumber
    fromRep (DynNumber i) = do
        (i' :: Int) <- toIntegral i
        case i' of
          0 -> return False
          1 -> return True
          _ -> Nothing
    toRep b = DynNumber (if b then 1 else 0)



-- | Type wrapper for binary data to be written to DynamoDB. Wrap any
-- 'Serialize' instance in there and 'DynVal' will know how to
-- automatically handle conversions in binary form.
newtype Bin a = Bin a deriving (Eq,Show,Read,Ord)


instance (Ser.Serialize a) => DynVal (Bin a) where
    type DynRep (Bin a) = DynBinary
    toRep (Bin i) = DynBinary (Ser.encode i)
    fromRep (DynBinary i) = either (const Nothing) (Just . Bin) $
                            Ser.decode i



-------------------------------------------------------------------------------
-- | Encode a Haskell value.
toValue :: DynVal a  => a -> DValue
toValue a = fromData $ toRep a


-------------------------------------------------------------------------------
-- | Decode a Haskell value.
fromValue :: DynVal a => DValue -> Maybe a
fromValue d = toData d >>= fromRep


toIntegral :: (Integral a, RealFrac a1) => a1 -> Maybe a
toIntegral sc = Just $ floor sc



-- | Value types natively recognized by DynamoDb. We pretty much
-- exactly reflect the AWS API onto Haskell types.
data DValue
    = DNum Scientific
    | DString T.Text
    | DBinary B.ByteString
    -- ^ Binary data will automatically be base64 marshalled.
    | DNumSet (S.Set Scientific)
    | DStringSet (S.Set T.Text)
    | DBinSet (S.Set B.ByteString)
    -- ^ Binary data will automatically be base64 marshalled.
    deriving (Eq,Show,Read,Ord)


instance IsString DValue where
    fromString t = DString (T.pack t)

-------------------------------------------------------------------------------
-- | Primary keys consist of either just a Hash key (mandatory) or a
-- hash key and a range key (optional).
data PrimaryKey = PrimaryKey {
      pkHash  :: Attribute
    , pkRange :: Maybe Attribute
    } deriving (Read,Show,Ord,Eq,Typeable)


-------------------------------------------------------------------------------
-- | Construct a hash-only primary key.
--
-- >>> hk "user-id" "ABCD"
--
-- >>> hk "user-id" (mkVal 23)
hk :: T.Text -> DValue -> PrimaryKey
hk k v = PrimaryKey (attr k v) Nothing


-------------------------------------------------------------------------------
-- | Construct a hash-and-range primary key.
hrk :: T.Text                   -- ^ Hash key name
    -> DValue                   -- ^ Hash key value
    -> T.Text                   -- ^ Range key name
    -> DValue                   -- ^ Range key value
    -> PrimaryKey
hrk k v k2 v2 = PrimaryKey (attr k v) (Just (attr k2 v2))


instance ToJSON PrimaryKey where
    toJSON (PrimaryKey h Nothing) = toJSON h
    toJSON (PrimaryKey h (Just r)) =
      let Object p1 = toJSON h
          Object p2 = toJSON r
      in Object (p1 `HM.union` p2)


-- | A key-value pair
data Attribute = Attribute {
      attrName :: T.Text
    , attrVal  :: DValue
    } deriving (Read,Show,Ord,Eq,Typeable)


-- | Convert attribute to a tuple representation
attrTuple :: Attribute -> (T.Text, DValue)
attrTuple (Attribute a b) = (a,b)


-- | Convenience function for constructing key-value pairs
attr :: DynVal a => T.Text -> a -> Attribute
attr k v = Attribute k (toValue v)


-- | 'attr' with type witness to help with cases where you're manually
-- supplying values in code.
--
-- >> item [ attrAs text "name" "john" ]
attrAs :: DynVal a => Proxy a -> T.Text -> a -> Attribute
attrAs _ k v = attr k v


-- | Type witness for 'Text'. See 'attrAs'.
text :: Proxy T.Text
text = Proxy


-- | Type witness for 'Integer'. See 'attrAs'.
int :: Proxy Integer
int = Proxy


-- | Type witness for 'Double'. See 'attrAs'.
double :: Proxy Double
double = Proxy


-- | A DynamoDb object is simply a key-value dictionary.
type Item = M.Map T.Text DValue


-------------------------------------------------------------------------------
-- | Pack a list of attributes into an Item.
item :: [Attribute] -> Item
item = M.fromList . map attrTuple


showT :: Show a => a -> T.Text
showT = T.pack . show

instance ToJSON DValue where
    toJSON (DNum i) = object ["N" .= showT i]
    toJSON (DString i) = object ["S" .= i]
    toJSON (DBinary i) = object ["B" .= (T.decodeUtf8 $ Base64.encode i)]
    toJSON (DNumSet i) = object ["NS" .= map showT (S.toList i)]
    toJSON (DStringSet i) = object ["SS" .= S.toList i]
    toJSON (DBinSet i) = object ["BS" .= map (T.decodeUtf8 . Base64.encode) (S.toList i)]
    toJSON x = error $ "aws: bug: DynamoDB can't handle " ++ show x


instance FromJSON DValue where
    parseJSON o = do
      (obj :: [(T.Text, Value)]) <- M.toList `liftM` parseJSON o
      case obj of
        [("N", numStr)] -> DNum <$> parseScientific numStr
        [("S", str)] -> DString <$> parseJSON str
        [("B", bin)] -> do
            res <- (Base64.decode . T.encodeUtf8) <$> parseJSON bin
            either fail (return . DBinary) res
        [("NS", s)] -> do xs <- mapM parseScientific =<< parseJSON s
                          return $ DNumSet $ S.fromList xs
        [("SS", s)] -> DStringSet <$> parseJSON s
        [("BS", s)] -> do
            xs <- mapM (either fail return . Base64.decode . T.encodeUtf8)
                  =<< parseJSON s
            return $ DBinSet $ S.fromList xs

        x -> fail $ "aws: unknown dynamodb value: " ++ show x

      where
        parseScientific str =
            ((fromIntegral :: Int64 -> Scientific) <$> parseJSON str) <|>
            ((fromFloatDigits :: Double -> Scientific) <$> parseJSON str)


instance ToJSON Attribute where
    toJSON (Attribute nm v) = object [nm .= v]


-------------------------------------------------------------------------------
-- | Errors defined by AWS.
data DdbErrCode
    = AccessDeniedException
    | ConditionalCheckFailedException
    | IncompleteSignatureException
    | InvalidSignatureException
    | LimitExceededException
    | MissingAuthenticationTokenException
    | ProvisionedThroughputExceededException
    | ResourceInUseException
    | ResourceNotFoundException
    | ThrottlingException
    | ValidationException
    | RequestTooLarge
    | InternalFailure
    | InternalServerError
    | ServiceUnavailableException
    | SerializationException
    -- ^ Raised by AWS when the request JSON is missing fields or is
    -- somehow malformed.
    deriving (Read,Show,Eq,Typeable)

-------------------------------------------------------------------------------
-- | Errors related to this library.
data DdbLibraryError
    = UnknownDynamoErrCode T.Text
    -- ^ A DynamoDB error code we do not know about.
    | JsonProtocolError Value T.Text
    -- ^ A JSON response we could not parse.
    deriving (Show,Eq,Typeable)


-- | Potential errors raised by DynamoDB
data DdbError = DdbError {
      ddbStatusCode :: Int
    -- ^ 200 if successful, 400 for client errors and 500 for
    -- server-side errors.
    , ddbErrCode    :: DdbErrCode
    , ddbErrMsg     :: T.Text
    } deriving (Show,Eq,Typeable)


instance C.Exception DdbError
instance C.Exception DdbLibraryError


-- | Response metadata that is present in every DynamoDB response.
data DdbResponse = DdbResponse {
      ddbrCrc   :: Maybe T.Text
    , ddbrMsgId :: Maybe T.Text
    }


instance Loggable DdbResponse where
    toLogText (DdbResponse id2 rid) =
        "DynamoDB: request ID=" `mappend`
        fromMaybe "<none>" rid `mappend`
        ", x-amz-id-2=" `mappend`
        fromMaybe "<none>" id2

instance Monoid DdbResponse where
    mempty = DdbResponse Nothing Nothing
    mappend a b = DdbResponse (ddbrCrc a `mplus` ddbrCrc b) (ddbrMsgId a `mplus` ddbrMsgId b)


data Region = Region {
      rUri  :: B.ByteString
    , rName :: B.ByteString
    } deriving (Eq,Show)


data DdbConfiguration qt = DdbConfiguration {
      ddbcRegion   :: Region
    -- ^ The regional endpoint. Ex: 'ddbUsEast'
    , ddbcProtocol :: Protocol
    -- ^ 'HTTP' o  r 'HTTPS'
    } deriving (Show)


instance DefaultServiceConfiguration (DdbConfiguration NormalQuery) where
  defServiceConfig = ddbHttps ddbUsEast1
  debugServiceConfig = ddbHttp ddbUsEast1


ddbUsEast1 :: Region
ddbUsEast1 = Region "dynamodb.us-east-1.amazonaws.com" "us-east-1"

ddbUsWest1 :: Region
ddbUsWest1 = Region "dynamodb.us-west-1.amazonaws.com" "us-west-1"

ddbUsWest2 :: Region
ddbUsWest2 = Region "dynamodb.us-west-2.amazonaws.com" "us-west-2"

ddbEuWest1 :: Region
ddbEuWest1 = Region "dynamodb.eu-west-1.amazonaws.com" "us-west-1"

ddbApNe1 :: Region
ddbApNe1 = Region "dynamodb.ap-northeast-1.amazonaws.com" "ap-northeast-1"

ddbApSe1 :: Region
ddbApSe1 = Region "dynamodb.ap-southeast-1.amazonaws.com" "ap-southeast-1"

ddbApSe2 :: Region
ddbApSe2 = Region "dynamodb.ap-southeast-2.amazonaws.com" "ap-southeast-2"

ddbSaEast1 :: Region
ddbSaEast1 = Region "dynamodb.sa-east-1.amazonaws.com" "sa-east-1"

ddbHttp :: Region -> DdbConfiguration NormalQuery
ddbHttp endpoint = DdbConfiguration endpoint HTTP

ddbHttps :: Region -> DdbConfiguration NormalQuery
ddbHttps endpoint = DdbConfiguration endpoint HTTPS


ddbSignQuery
    :: A.ToJSON a
    => B.ByteString
    -> a
    -> DdbConfiguration qt
    -> SignatureData
    -> SignedQuery
ddbSignQuery target body di sd
    = SignedQuery {
        sqMethod = Post
      , sqProtocol = ddbcProtocol di
      , sqHost = host
      , sqPort = defaultPort (ddbcProtocol di)
      , sqPath = "/"
      , sqQuery = []
      , sqDate = Just $ signatureTime sd
      , sqAuthorization = Just auth
      , sqContentType = Just "application/x-amz-json-1.0"
      , sqContentMd5 = Nothing
      , sqAmzHeaders = [ ("X-Amz-Target", dyApiVersion <> target)
                       , ("X-Amz-Date", sigTime)
                       ]
      , sqOtherHeaders = []
      , sqBody = Just $ HTTP.RequestBodyLBS bodyLBS
      , sqStringToSign = canonicalRequest
      }
    where

        Region{..} = ddbcRegion di
        host = rUri

        sigTime = fmtTime "%Y%m%dT%H%M%SZ" $ signatureTime sd

        bodyLBS = A.encode body
        bodyHash = Base16.encode $ toBytes (hashlazy bodyLBS :: Digest SHA256)

        canonicalRequest = B.concat [ "POST\n"
                                    , "/\n"
                                    , "\n" -- query string
                                    , "content-type:application/x-amz-json-1.0\n"
                                    , "host:"
                                    , host
                                    , "\n"
                                    , "x-amz-date:"
                                    , sigTime
                                    , "\n"
                                    , "x-amz-target:"
                                    , dyApiVersion
                                    , target
                                    , "\n"
                                    , "\n" -- end headers
                                    , "content-type;host;x-amz-date;x-amz-target\n"
                                    , bodyHash
                                    ]

        auth = authorizationV4 sd HmacSHA256 rName "dynamodb"
                               "content-type;host;x-amz-date;x-amz-target"
                               canonicalRequest

data AmazonError = AmazonError {
      aeType    :: T.Text
    , aeMessage :: Maybe T.Text
    }

instance FromJSON AmazonError where
    parseJSON (Object v) = AmazonError
        <$> v .: "__type"
        <*> v .:? "message"
    parseJSON _ = error $ "aws: unexpected AmazonError message"




-------------------------------------------------------------------------------
ddbResponseConsumer :: A.FromJSON a => IORef DdbResponse -> HTTPResponseConsumer a
ddbResponseConsumer ref resp = do
    val <- HTTP.responseBody resp $$+- sinkParser A.json'
    case statusCode of
      200 -> rSuccess val
      _   -> rError val
  where

    header = fmap T.decodeUtf8 . flip lookup (HTTP.responseHeaders resp)
    amzId = header "x-amzn-RequestId"
    amzCrc = header "x-amz-crc32"
    meta = DdbResponse amzCrc amzId
    tellMeta = liftIO $ tellMetadataRef ref meta

    rSuccess val =
      case A.fromJSON val of
        A.Success a -> return a
        A.Error err -> do
            tellMeta
            throwM $ JsonProtocolError val (T.pack err)

    rError val = do
      tellMeta
      case parseEither parseJSON val of
        Left e ->
          throwM $ JsonProtocolError val (T.pack e)

        Right err'' -> do
          let e = T.drop 1 . snd . T.breakOn "#" $ aeType err''
          errCode <- readErrCode e
          throwM $ DdbError statusCode errCode (fromMaybe "" $ aeMessage err'')

    readErrCode txt =
        let txt' = T.unpack txt
        in case readMay txt' of
             Just e -> return $ e
             Nothing -> throwM (UnknownDynamoErrCode txt)

    HTTP.Status{..} = HTTP.responseStatus resp


-- | Conditions used by mutation operations ('PutItem', 'UpdateItem',
-- etc.). The default 'def' instance is empty (no condition).
data Expects = Expects CondOp [Expect]
    deriving (Eq,Show,Read,Ord,Typeable)

instance Default Expects where
    def = Expects CondAnd []

expectsJson :: Expects -> [Pair]
expectsJson (Expects op es) = b ++ a
    where
      a = if null es
          then []
          else ["Expected" .= object (map expectJson es)]

      b = if length (take 2 es) > 1
          then ["ConditionalOperator" .= String (rendCondOp op) ]
          else []

-------------------------------------------------------------------------------
rendCondOp :: CondOp -> T.Text
rendCondOp CondAnd = "AND"
rendCondOp CondOr = "OR"


-------------------------------------------------------------------------------
-- | Logical operator for merging multiple conditions.
data CondOp = CondAnd | CondOr
    deriving (Eq,Show,Read,Ord,Typeable)


-- | A condition used by mutation operations ('PutItem', 'UpdateItem', etc.).
data Expect = Expect {
      expectAttr      :: T.Text
    -- ^ Attribute to use as the basis for this conditional
    , expectCondition :: Condition
    } deriving (Eq,Show,Read,Ord,Typeable)


-------------------------------------------------------------------------------
-- | Conditional check for a given attribute.
data Condition
    = DEq DValue
    | NotEq DValue
    | DLE DValue
    | DLT DValue
    | DGE DValue
    | DGT DValue
    | NotNull
    | IsNull
    | Contains DValue
    | NotContains DValue
    | Begins DValue
    | In [DValue]
    | Between DValue DValue
    deriving (Eq,Show,Read,Ord,Typeable)


-------------------------------------------------------------------------------
getCondValues :: Condition -> [DValue]
getCondValues c = case c of
    DEq v -> [v]
    NotEq v -> [v]
    DLE v -> [v]
    DLT v -> [v]
    DGE v -> [v]
    DGT v -> [v]
    NotNull -> []
    IsNull -> []
    Contains v -> [v]
    NotContains v -> [v]
    Begins v -> [v]
    In v -> v
    Between a b -> [a,b]


-------------------------------------------------------------------------------
renderCondOp :: Condition -> T.Text
renderCondOp c = case c of
    DEq{} -> "EQ"
    NotEq{} -> "NE"
    DLE{} -> "LE"
    DLT{} -> "LT"
    DGE{} -> "GE"
    DGT{} -> "GT"
    NotNull -> "NOT_NULL"
    IsNull -> "NULL"
    Contains{} -> "CONTAINS"
    NotContains{} -> "NOT_CONTAINS"
    Begins{} -> "BEGINS_WITH"
    In{} -> "IN"
    Between{} -> "BETWEEN"


expectJson :: Expect -> Pair
expectJson Expect{..} = expectAttr .= expectCondition


instance ToJSON Condition where
    toJSON c = object
      [ "ComparisonOperator" .= String (renderCondOp c)
      , "AttributeValueList" .= getCondValues c ]


-------------------------------------------------------------------------------
dyApiVersion :: B.ByteString
dyApiVersion = "DynamoDB_20120810."



-------------------------------------------------------------------------------
-- | The standard response metrics on capacity consumption.
data ConsumedCapacity = ConsumedCapacity {
      capacityUnits       :: Int64
    , capacityGlobalIndex :: [(T.Text, Int64)]
    , capacityLocalIndex  :: [(T.Text, Int64)]
    , capacityTableUnits  :: Maybe Int64
    , capacityTable       :: T.Text
    } deriving (Eq,Show,Read,Ord,Typeable)


instance FromJSON ConsumedCapacity where
    parseJSON (Object v) = ConsumedCapacity
      <$> v .: "CapacityUnits"
      <*> (HM.toList <$> v .:? "GlobalSecondaryIndexes" .!= mempty)
      <*> (HM.toList <$> v .:? "LocalSecondaryIndexes" .!= mempty)
      <*> (v .:? "Table" >>= maybe (return Nothing) (.: "CapacityUnits"))
      <*> v .: "TableName"
    parseJSON _ = fail "ConsumedCapacity must be an Object."



data ReturnConsumption = RCIndexes | RCTotal | RCNone
    deriving (Eq,Show,Read,Ord,Typeable)

instance ToJSON ReturnConsumption where
    toJSON RCIndexes = String "INDEXES"
    toJSON RCTotal = String "TOTAL"
    toJSON RCNone = String "NONE"

instance Default ReturnConsumption where
    def = RCNone

data ReturnItemCollectionMetrics = RICMSize | RICMNone
    deriving (Eq,Show,Read,Ord,Typeable)

instance ToJSON ReturnItemCollectionMetrics where
    toJSON RICMSize = String "SIZE"
    toJSON RICMNone = String "NONE"

instance Default ReturnItemCollectionMetrics where
    def = RICMNone


data ItemCollectionMetrics = ItemCollectionMetrics {
      icmKey      :: (T.Text, DValue)
    , icmEstimate :: [Double]
    } deriving (Eq,Show,Read,Ord,Typeable)


instance FromJSON ItemCollectionMetrics where
    parseJSON (Object v) = ItemCollectionMetrics
      <$> (do m <- v .: "ItemCollectionKey"
              return $ head $ HM.toList m)
      <*> v .: "SizeEstimateRangeGB"
    parseJSON _ = fail "ItemCollectionMetrics must be an Object."


-------------------------------------------------------------------------------
-- | What to return from the current update operation
data UpdateReturn
    = URNone                    -- ^ Return nothing
    | URAllOld                  -- ^ Return old values
    | URUpdatedOld              -- ^ Return old values with a newer replacement
    | URAllNew                  -- ^ Return new values
    | URUpdatedNew              -- ^ Return new values that were replacements
    deriving (Eq,Show,Read,Ord,Typeable)


instance ToJSON UpdateReturn where
    toJSON URNone = toJSON (String "NONE")
    toJSON URAllOld = toJSON (String "ALL_OLD")
    toJSON URUpdatedOld = toJSON (String "UPDATED_OLD")
    toJSON URAllNew = toJSON (String "ALL_NEW")
    toJSON URUpdatedNew = toJSON (String "UPDATED_NEW")


instance Default UpdateReturn where
    def = URNone

