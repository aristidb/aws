{-# LANGUAGE DeriveDataTypeable        #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE ScopedTypeVariables       #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Aws.DynaboDb.Core
-- Copyright   :  Ozgun Ataman, Soostone Inc.
-- License     :  BSD3
--
-- Maintainer  :  Ozgun Ataman <oz@soostone.com>
-- Stability   :  experimental
--
----------------------------------------------------------------------------

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
    , IsDVal (..)
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

    ) where


-------------------------------------------------------------------------------
import qualified Blaze.ByteString.Builder       as Blaze
import qualified Blaze.ByteString.Builder.Char8 as Blaze8
import           Control.Applicative
import           Control.Arrow
import qualified Control.Exception              as C
import           Control.Monad
import           Control.Monad.IO.Class
import           Crypto.Classes                 (Hash (..))
import           Crypto.Hash                    (Digest, SHA256,
                                                 digestToHexByteString, hash)
import qualified Crypto.Hash.SHA256             as SHA256
import           Crypto.HMAC                    (MacKey (..), hmac')
import           Data.Aeson                     (FromJSON (..), ToJSON (..),
                                                 Value (..), json', object,
                                                 parseJSON, toJSON, (.:), (.=))
import qualified Data.Aeson                     as A
import           Data.Aeson.Types               (parseEither)
import qualified Data.ByteString.Base16         as Base16
import qualified Data.ByteString.Base64         as Base64
import qualified Data.ByteString.Char8          as B
import qualified Data.ByteString.Lazy.Char8     as LB
import           Data.CaseInsensitive           (mk)
import           Data.Conduit
import           Data.Conduit.Attoparsec        (sinkParser)
import           Data.Conduit.List              (consume)
import           Data.IORef
import           Data.List
import qualified Data.Map                       as M
import           Data.Maybe
import           Data.Monoid
import           Data.Ord
import qualified Data.Serialize                 as Ser
import qualified Data.Set                       as S
import qualified Data.Text                      as T
import qualified Data.Text.Encoding             as T
import           Data.Time
import           Data.Typeable
-- import           Debug.Trace
import           Network.HTTP.Conduit
import qualified Network.HTTP.Conduit           as HTTP
import qualified Network.HTTP.Types             as HTTP
import           Safe
import           System.Locale
-------------------------------------------------------------------------------
import           Aws.Core
-------------------------------------------------------------------------------


-- | Class of native Haskell types that can be converted to DynamoDB
-- types using this common interface. DynamoDB is typed and this class
-- helps us work with that through a more convenient interface.
class IsDVal a where
    toDVal :: a -> DValue
    fromDVal :: DValue -> Maybe a


instance IsDVal Int where
    toDVal i = DInt (fromIntegral i)
    fromDVal (DInt i) = Just $ fromIntegral i
    fromDVal _ = Nothing


instance IsDVal Integer where
    toDVal i = DInt i
    fromDVal (DInt i) = Just i
    fromDVal _ = Nothing


instance IsDVal Double where
    toDVal i = DDouble i
    fromDVal (DDouble i) = Just i
    fromDVal _ = Nothing


instance IsDVal T.Text where
    toDVal i = DString i
    fromDVal (DString x) = Just x
    fromDVal _ = Nothing


instance IsDVal [T.Text] where
    toDVal i = DStringSet $ S.fromList i
    fromDVal (DStringSet x) = Just $ S.toList x
    fromDVal _ = Nothing


instance IsDVal (S.Set T.Text) where
    toDVal i = DStringSet i
    fromDVal (DStringSet x) = Just x
    fromDVal _ = Nothing


instance IsDVal [Double] where
    toDVal i = DDoubleSet $ S.fromList i
    fromDVal (DDoubleSet x) = Just $ S.toList x
    fromDVal _ = Nothing


instance IsDVal (S.Set Double) where
    toDVal i = DDoubleSet i
    fromDVal (DDoubleSet x) = Just x
    fromDVal _ = Nothing


instance IsDVal [Int] where
    toDVal i = DIntSet $ S.fromList $ map fromIntegral i
    fromDVal (DIntSet x) = Just $ map fromIntegral $ S.toList x
    fromDVal _ = Nothing


instance IsDVal (S.Set Int) where
    toDVal i = DIntSet $ S.map fromIntegral i
    fromDVal (DIntSet x) = Just $ S.map fromIntegral x
    fromDVal _ = Nothing


instance IsDVal (S.Set Integer) where
    toDVal i = DIntSet i
    fromDVal (DIntSet x) = Just x
    fromDVal _ = Nothing


-- | Type wrapper for binary data to be written to DynamoDB. Wrap any
-- 'Serialize' instance in there and 'IsDVal' will know how to
-- automatically handle conversions in binary form.
newtype Bin a = Bin a deriving (Eq,Show,Read,Ord)


instance Ser.Serialize a => IsDVal (Bin a) where
    toDVal (Bin a) = DBinary (Base64.encode (Ser.encode a))
    fromDVal (DBinary x) = either (const Nothing) (Just . Bin) $
                           Ser.decode =<< Base64.decode x
    fromDVal _ = Nothing



-- | Convenience to construct DynamoDB values from Haskell types. Just
-- a synonym for 'toDVal'.
mkVal :: IsDVal a => a -> DValue
mkVal = toDVal


-- | A value as defined/recognized by DynamoDB. We split into more
-- types to have this work more natively with Haskell.
data DValue
    = DInt Integer
    | DDouble Double
    | DString T.Text
    | DBinary B.ByteString
    | DIntSet (S.Set Integer)
    | DDoubleSet (S.Set Double)
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
hpk :: IsDVal a => a -> PrimaryKey
hpk a = HPK $ mkVal a


-- | Make a composite primary key from a hash attribute and a range
-- attribute.
hrpk :: (IsDVal a, IsDVal b) => a -> b -> PrimaryKey
hrpk a b = HRPK (mkVal a) (mkVal b)


-- | A key-value pair
type Attribute = (T.Text, DValue)


-- | Convenience function for constructing key-value pairs
attr :: IsDVal a => T.Text -> a -> (T.Text, DValue)
attr k v = (k, mkVal v)


-- | 'attr' with type witness to help with cases where you're manually
-- supplying values in code.
--
-- >> item [ attrAs text "name" "john"
attrAs :: IsDVal a => a -> T.Text -> a -> (T.Text, DValue)
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


-- | Haskell data structure representing a single fetched item from
-- DynamoDB.
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
    toJSON (DInt i) = object ["N" .= showT i]
    toJSON (DDouble i) = object ["N" .= showT i]
    toJSON (DString i) = object ["S" .= i]
    toJSON (DBinary i) = object ["B" .= (T.decodeUtf8 $ Base64.encode i)]
    toJSON (DIntSet i) = object ["NS" .= map showT (S.toList i)]
    toJSON (DDoubleSet i) = object ["NS" .= map showT (S.toList i)]
    toJSON (DStringSet i) = object ["SS" .= S.toList i]
    toJSON (DBinSet i) = object ["BS" .= map (T.decodeUtf8 . Base64.encode) (S.toList i)]
    toJSON x = error $ "aws: bug: DynamoDB can't handle " ++ show x


instance FromJSON DValue where
    parseJSON o = do
      (obj :: [(T.Text, Value)]) <- M.toList `liftM` parseJSON o
      case obj of
        [("N", numStr)] -> parseNum numStr
        [("S", str)] -> DString <$> parseJSON str
        [("B", bin)] -> do
            res <- (Base64.decode . T.encodeUtf8) <$> parseJSON bin
            either fail (return . DBinary) res
        [("NS", s)] -> (DIntSet <$> parseJSON s) <|> (DDoubleSet <$> parseJSON s)
        [("SS", s)] -> undefined
        [("BS", s)] -> undefined
        x -> fail $ "aws: unknown dynamodb value: " ++ show x

      where
        parseNum str =
          (DInt <$> parseJSON str) <|> (DDouble <$> parseJSON str)


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
      ddbrCrc :: Maybe T.Text
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
    mappend a b = DdbResponse
                   (ddbrCrc a `mplus` ddbrCrc b)
                   (ddbrMsgId a `mplus` ddbrMsgId b)


data Region = Region {
      rUri :: B.ByteString
    , rName :: B.ByteString
    } deriving (Eq,Show)


data DdbConfiguration qt = DdbConfiguration {
      ddbcRegion :: Region
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



ddbSignQuery :: A.ToJSON a
             => a
             -- ^ The request/payload
             -> B.ByteString
             -- ^ Targeted action
             -> DdbConfiguration qt
             -- ^ Configuration
             -> SignatureData
             -- ^ signature metadata
             -> SignedQuery
ddbSignQuery msg target conf sd@SignatureData{..} = SignedQuery {
        sqMethod = method
      , sqProtocol = ddbcProtocol conf
      , sqHost = host
      , sqPort = defaultPort (ddbcProtocol conf)
      , sqPath = canUri
      , sqQuery = []
      , sqDate = Just signatureTime
      , sqAuthorization = Just authHeader
      , sqContentType = Just "application/x-amz-json-1.0"
      , sqContentMd5 = Nothing
      , sqAmzHeaders = allHeaders
      , sqOtherHeaders = []
      , sqBody = Just $ RequestBodyBS payload
      , sqStringToSign = strToSign
      }
    where
      allHeaders = filter ((/= "content-type") . fst) $ map (first mk) headers

      Region{..} = ddbcRegion conf
      host = rUri

      method = PostQuery
      canUri = "/"
      canQuery = ""
      headers = sortBy (comparing fst)
        [ ("host", host)
        , ("content-type", "application/x-amz-json-1.0")
        , ("x-amz-date", rqDateTime)
        , ("x-amz-target", amzTarget)
        ]

      -- | Use unlines here because we want a newline per header, even
      -- on the last in the list.
      canHeaders = B.unlines $ map mkCanHeader headers
      mkCanHeader (h,v) = B.concat [h, ":", v]

      canReq = B.intercalate "\n"
        [ httpMethod method
        , canUri
        , canQuery
        , canHeaders
        , signedHeaders
        , hpayload ]

      payload = B.concat . LB.toChunks $ A.encode msg
      hpayload = digestToHexByteString $ (hash payload :: Digest SHA256)

      hCanReq = -- trace ("CanReq: " ++ show canReq) $
                digestToHexByteString $ (hash canReq :: Digest SHA256)

      algo = "AWS4-HMAC-SHA256"

      -- | Everybody needs date in a different format!
      rqDateTime = B.pack $ formatTime defaultTimeLocale "%Y%m%dT%H%M%SZ" signatureTime
      credDate = B.pack $ formatTime defaultTimeLocale "%Y%m%d" signatureTime

      credScope = B.intercalate "/" [credDate, rName, "dynamodb", "aws4_request"]
      strToSign = B.intercalate "\n" [algo, rqDateTime, credScope, hCanReq]

      hmac'' :: B.ByteString -> B.ByteString -> B.ByteString
      hmac'' k v = Ser.encode $ hmac' f v
          where
            f :: MacKey SHA256.Ctx SHA256.SHA256
            f = MacKey k

      Credentials{..} = signatureCredentials
      kSecret = secretAccessKey
      kDate = hmac'' (B.concat ["AWS4", kSecret]) credDate
      kRegion = hmac'' kDate rName
      kService = hmac'' kRegion "dynamodb"

      kSigning :: B.ByteString
      kSigning = hmac'' kService "aws4_request"

      sig :: B.ByteString
      sig = -- trace ("StrToSign: " ++ show strToSign) $
            Base16.encode $ hmac'' kSigning strToSign

      amzTarget = B.concat ["DynamoDB_20111205.", target]
      cred = B.intercalate "/" [accessKeyID, credScope]
      authHeader = B.concat
          [ algo, " ", "Credential=", cred, ","
          , "SignedHeaders=", signedHeaders, ","
          , "Signature=", sig]
      signedHeaders = B.intercalate ";" $ map fst headers



data AmazonError = AmazonError {
      aeType :: T.Text
    , aeMessage :: T.Text
    }

instance FromJSON AmazonError where
    parseJSON (Object v) = AmazonError
        <$> v .: "__type"
        <*> v .: "message"
    parseJSON _ = error $ "aws: unexpected AmazonError message"



ddbResponseConsumer :: (MonadIO m, MonadThrow m, FromJSON b)
                    => IORef DdbResponse
                    -> HTTP.Response (ResumableSource m B.ByteString)
                    -> m b
ddbResponseConsumer ref HTTP.Response{..} = do
    case statusCode of
      200 -> rSuccess
      400 -> rError
      404 -> do
        body <- responseBody $$+- consume
        error (B.unpack $ B.concat body)
      413 -> rError
      500 -> rError
      x -> error $ "aws: unknown return code: " ++ show x

    where
      header = fmap T.decodeUtf8 . flip lookup responseHeaders
      amzId = header "x-amzn-RequestId"
      amzCrc = header "x-amz-crc32"
      meta = DdbResponse amzCrc amzId

      rSuccess = do
        res <- responseBody $$+- sinkParser json'
        let res' = parseEither parseJSON res
        case res' of
          Left e -> error $ "aws: Could not parse successful result from DynamoDB: " ++ e
          Right res'' -> do
            liftIO $ tellMetadataRef ref meta
            return res''

      rError = do
        err <- responseBody $$+- sinkParser json'
        let err' = parseEither parseJSON err
        case err' of
          Left e -> error "aws: Could not parse error message from DynamoDB"
          Right err'' -> do
            let e = T.drop 1 . snd . T.breakOn "#" $ aeType err''
                ddbErr = DdbError statusCode (convErr e) (aeMessage err'')
            monadThrow ddbErr

      convErr txt =
          let txt' = T.unpack txt
          in case readMay txt' of
               Just e -> e
               Nothing -> error txt'

      HTTP.Status{..} = responseStatus

