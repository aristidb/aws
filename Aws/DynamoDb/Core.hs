{-# LANGUAGE DeriveDataTypeable        #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE ScopedTypeVariables       #-}

module Aws.DynamoDb.Core
    (
    -- * Configuration & Regions
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

    -- * DynamoDB Types
    , DValue (..)
    , DVal (..)
    , PrimaryKey (..)
    , Attribute
    , Bin (..)
    , Item (..)
    , defItem
    , mkVal
    , hpk
    , hrpk
    , attr
    , attrAs
    , text, int, double
    , item

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

    , Expect (..)

    ) where


-------------------------------------------------------------------------------
import           Control.Applicative
import qualified Control.Exception            as C
import           Control.Monad
import           Control.Monad.Trans
import           Control.Monad.Trans.Resource (throwM)
import           Crypto.Hash
import           Data.Aeson                   (FromJSON (..), ToJSON (..),
                                               Value (..), object, parseJSON,
                                               toJSON, (.:), (.=))
import qualified Data.Aeson                   as A
import           Data.Aeson.Types             (parseEither)
import           Data.Byteable
import qualified Data.ByteString.Base16       as Base16
import qualified Data.ByteString.Base64       as Base64
import qualified Data.ByteString.Char8        as B
import           Data.Conduit
import           Data.Conduit.Attoparsec      (sinkParser)
import           Data.Int
import           Data.IORef
import qualified Data.Map                     as M
import           Data.Maybe
import           Data.Monoid
import           Data.Scientific
import qualified Data.Serialize               as Ser
import qualified Data.Set                     as S
import qualified Data.Text                    as T
import qualified Data.Text.Encoding           as T
import           Data.Typeable
-- import           Debug.Trace
import           Network.HTTP.Conduit
import qualified Network.HTTP.Conduit         as HTTP
import qualified Network.HTTP.Types           as HTTP
import           Safe
-------------------------------------------------------------------------------
import           Aws.Core
-------------------------------------------------------------------------------


-- | Class of native Haskell types that can be converted to DynamoDB
-- types using this common interface. DynamoDB is typed and this class
-- helps us work with that through a more convenient interface.
class DVal a where
    toDVal :: a -> DValue
    fromDVal :: DValue -> Maybe a


instance DVal Int where
    toDVal i = DNum (fromIntegral i)
    fromDVal (DNum i) = toIntegral i
    fromDVal _ = Nothing


instance DVal Integer where
    toDVal i = DNum (fromIntegral i)
    fromDVal (DNum i) = toIntegral i
    fromDVal _ = Nothing


instance DVal Double where
    toDVal i = DNum (fromFloatDigits i)
    fromDVal (DNum i) = Just (toRealFloat i)
    fromDVal _ = Nothing


instance DVal T.Text where
    toDVal i = DString i
    fromDVal (DString x) = Just x
    fromDVal _ = Nothing


instance DVal [T.Text] where
    toDVal i = DStringSet $ S.fromList i
    fromDVal (DStringSet x) = Just $ S.toList x
    fromDVal _ = Nothing


instance DVal (S.Set T.Text) where
    toDVal i = DStringSet i
    fromDVal (DStringSet x) = Just x
    fromDVal _ = Nothing


instance DVal [Double] where
    toDVal i = DNumSet $ S.fromList $ map fromFloatDigits i
    fromDVal (DNumSet x) = Just $ map toRealFloat $ S.toList x
    fromDVal _ = Nothing


instance DVal (S.Set Double) where
    toDVal i = DNumSet $ S.map fromFloatDigits i
    fromDVal (DNumSet x) = Just $ S.map toRealFloat x
    fromDVal _ = Nothing


instance DVal [Int] where
    toDVal i = DNumSet $ S.fromList $ map fromIntegral i
    fromDVal (DNumSet x) = sequence $ map toIntegral $ S.toList x
    fromDVal _ = Nothing


instance DVal (S.Set Int) where
    toDVal i = DNumSet $ S.map fromIntegral i
    fromDVal ds@(DNumSet _) = S.fromList <$> fromDVal ds
    fromDVal _ = Nothing


toIntegral :: (Integral a, RealFrac a1) => a1 -> Maybe a
toIntegral sc = Just $ floor sc


-- | Type wrapper for binary data to be written to DynamoDB. Wrap any
-- 'Serialize' instance in there and 'DVal' will know how to
-- automatically handle conversions in binary form.
newtype Bin a = Bin a deriving (Eq,Show,Read,Ord)


instance Ser.Serialize a => DVal (Bin a) where
    toDVal (Bin a) = DBinary (Base64.encode (Ser.encode a))
    fromDVal (DBinary x) = either (const Nothing) (Just . Bin) $
                           Ser.decode =<< Base64.decode x
    fromDVal _ = Nothing



-- | Convenience to construct DynamoDB values from Haskell types. Just
-- a synonym for 'toDVal'.
mkVal :: DVal a => a -> DValue
mkVal = toDVal


-- | A value as defined/recognized by DynamoDB. We split into more
-- types to have this work more natively with Haskell.
data DValue
    = DNum Scientific
    | DString T.Text
    | DBinary B.ByteString
    | DNumSet (S.Set Scientific)
    | DStringSet (S.Set T.Text)
    | DBinSet (S.Set B.ByteString)
    deriving (Eq,Show,Read,Ord)


-- | A primary key recognized by DynamoDB. Used in many of the
-- DynamoDB operations.
data PrimaryKey
    = HPK { pkHashElem :: DValue }
    -- ^ When the key is just a hash primary key
    | HRPK { pkHashElem :: DValue, pkRangeElem :: DValue }
    -- ^ When the key is a hash-and-range primary key
    deriving (Eq,Show,Read,Ord)


-- | Make a primary key from a single value
--
-- Assuming @name@ is a primary hash key attribute:
-- >> hpk "john"
hpk :: DVal a => a -> PrimaryKey
hpk a = HPK $ mkVal a


-- | Make a composite primary key from a hash attribute and a range
-- attribute.
hrpk :: (DVal a, DVal b) => a -> b -> PrimaryKey
hrpk a b = HRPK (mkVal a) (mkVal b)


-- | A key-value pair
type Attribute = (T.Text, DValue)


-- | Convenience function for constructing key-value pairs
attr :: DVal a => T.Text -> a -> (T.Text, DValue)
attr k v = (k, mkVal v)


-- | 'attr' with type witness to help with cases where you're manually
-- supplying values in code.
--
-- >> item [ attrAs text "name" "john" ]
attrAs :: DVal a => a -> T.Text -> a -> (T.Text, DValue)
attrAs _ k v = attr k v


-- | Type witness for 'Text'. See 'attrAs'.
text :: T.Text
text = undefined


-- | Type witness for 'Integer'. See 'attrAs'.
int :: Integer
int = undefined


-- | Type witness for 'Double'. See 'attrAs'.
double :: Double
double = undefined


-- | Convenience function for constructing 'Item's from manually
-- entered key-value pairs.
--
-- >> item [ attr "name" name, attr "age" age]
item :: [Attribute] -> Item
item atts = Item $ M.fromList atts


-- | Haskell data structure representing a single fetched item/object
-- from DynamoDB.
newtype Item = Item { itemAttrs :: M.Map T.Text DValue }
    deriving (Eq,Show,Read,Ord)


-- | Empty item.
defItem :: Item
defItem = Item M.empty


instance FromJSON Item where
    parseJSON v = Item <$> parseJSON v
    parseJSON _ = fail "aws: failed while parsing Item"


instance ToJSON Item where
    toJSON (Item v) = toJSON v


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
        [("SS", _)] -> undefined
        [("BS", _)] -> undefined
        x -> fail $ "aws: unknown dynamodb value: " ++ show x

      where
        parseScientific str =
            ((fromIntegral :: Int64 -> Scientific) <$> parseJSON str) <|>
            ((fromFloatDigits :: Double -> Scientific) <$> parseJSON str)


instance ToJSON PrimaryKey where
    toJSON (HPK k) = object ["HashKeyElement" .= toJSON k]
    toJSON (HRPK k r) = object
        [ "HashKeyElement" .= toJSON k, "RangeKeyElement" .= toJSON r ]


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
    | UnknownDynamoError T.Text
    deriving (Read,Show,Eq,Typeable)


-- | Potential errors raised by DynamoDB
data DdbError = DdbError {
      ddbStatusCode :: Int
    , ddbErrCode    :: DdbErrCode
    , ddbErrMsg     :: T.Text
    } deriving (Show,Eq,Typeable)


instance C.Exception DdbError


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
    , ddbcRetries  :: Int
    -- ^ Number of times server errors should result in a retry.
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
ddbHttp endpoint = DdbConfiguration endpoint HTTP 3

ddbHttps :: Region -> DdbConfiguration NormalQuery
ddbHttps endpoint = DdbConfiguration endpoint HTTPS 3


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
    , aeMessage :: T.Text
    }

instance FromJSON AmazonError where
    parseJSON (Object v) = AmazonError
        <$> v .: "__type"
        <*> v .: "message"
    parseJSON _ = error $ "aws: unexpected AmazonError message"




-------------------------------------------------------------------------------
ddbResponseConsumer :: A.FromJSON a => IORef DdbResponse -> HTTPResponseConsumer a
ddbResponseConsumer ref resp = do
    val <- HTTP.responseBody resp $$+- sinkParser A.json'
    case statusCode of
      200 -> rSuccess val
      _   -> rError val
  where

    header = fmap T.decodeUtf8 . flip lookup (responseHeaders resp)
    amzId = header "x-amzn-RequestId"
    amzCrc = header "x-amz-crc32"
    meta = DdbResponse amzCrc amzId
    tellMeta = liftIO $ tellMetadataRef ref meta

    rSuccess val =
      case A.fromJSON val of
        A.Success a -> return a
        A.Error err -> do
            tellMeta
            throwM $ DdbError statusCode InternalFailure (T.pack err)

    rError val =
      case parseEither parseJSON val of
        Left e -> throwM $ DdbError statusCode InternalFailure (T.pack e)
        Right err'' -> do
          let e = T.drop 1 . snd . T.breakOn "#" $ aeType err''
              ddbErr = DdbError statusCode (convErr e) (aeMessage err'')
          tellMeta
          throwM ddbErr

    convErr txt =
        let txt' = T.unpack txt
        in case readMay txt' of
             Just e -> e
             Nothing -> UnknownDynamoError txt

    HTTP.Status{..} = responseStatus resp




-------------------------------------------------------------------------------
type Expects = [Expect]


-- | Perform 'PutItem' only if 'peExists' matches the reality for the
-- other parameters here.
data Expect = Expect {
      expectAttr   :: T.Text
    -- ^ Attribute for the existence check
    , expectVal    :: Maybe DValue
    -- ^ Further constrain this check and make it apply only if
    -- attribute has this value
    , expectExists :: Bool
    -- ^ If 'True', will only match if attribute exists. If 'False'
    -- will only match if the attribute is missing.
    } deriving (Eq,Show,Read,Ord)


instance ToJSON Expects where
    toJSON  = object . map mk
        where
          mk Expect{..} = expectAttr .= object sub
              where
                sub = maybe [] (return . ("Value" .= )) expectVal ++
                      ["Exists" .= expectExists]


dyApiVersion :: B.ByteString
dyApiVersion = "DynamoDB_20120810."
