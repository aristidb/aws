{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, TypeFamilies, FlexibleContexts, FlexibleInstances, DeriveFunctor, OverloadedStrings, RecordWildCards, DeriveDataTypeable #-}
module Aws.Core where

import           Control.Applicative
import           Control.Arrow
import           Control.Exception
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Control
import           Data.Attempt                (Attempt(..))
import           Data.ByteString             (ByteString)
import           Data.ByteString.Char8       ({- IsString -})
import           Data.Char
import           Data.Conduit                (Source, ResourceT, ($$))
import           Data.IORef
import           Data.List
import           Data.Maybe
import           Data.Monoid
import           Data.Time
import           Data.Typeable
import           Data.Word
import           System.Directory
import           System.Environment
import           System.FilePath             ((</>))
import           System.Locale
import           Text.XML.Cursor
import qualified Blaze.ByteString.Builder    as Blaze
import qualified Control.Exception           as E
import qualified Control.Exception.Lifted
import qualified Control.Failure             as F
import qualified Crypto.Classes              as Crypto
import qualified Crypto.HMAC                 as HMAC
import qualified Crypto.Hash.SHA1            as SHA1
import qualified Crypto.Hash.SHA256          as SHA256
import qualified Data.ByteString             as B
import qualified Data.ByteString.Base64      as Base64
import qualified Data.ByteString.Lazy        as L
import qualified Data.ByteString.UTF8        as BU
import qualified Data.Conduit                as C
import qualified Data.Conduit.List           as CL
import qualified Data.Serialize              as Serialize
import qualified Data.Text                   as T
import qualified Data.Text.Encoding          as T
import qualified Data.Text.IO                as T
import qualified Network.HTTP.Conduit        as HTTP
import qualified Network.HTTP.Types          as HTTP
import qualified Text.XML                    as XML
import qualified Text.XML.Cursor             as Cu

data Response m a = Response m (Attempt a)
    deriving (Show, Functor)

tellMetadata :: m -> Response m ()
tellMetadata m = Response m (return ())

instance Monoid m => Monad (Response m) where
    return x = Response mempty (Success x)
    Response m1 (Failure e) >>= _ = Response m1 (Failure e)
    Response m1 (Success x) >>= f = let Response m2 y = f x
                                    in Response (m1 `mappend` m2) y -- currently using First-semantics, Last SHOULD work too

instance (Monoid m, E.Exception e) => F.Failure e (Response m) where
    failure e = Response mempty (F.failure e)

tellMetadataRef :: Monoid m => IORef m -> m -> IO ()
tellMetadataRef r m = modifyIORef r (`mappend` m)

type HTTPResponseConsumer a =  HTTP.Status
                            -> HTTP.ResponseHeaders
                            -> Source (ResourceT IO) ByteString
                            -> ResourceT IO a

class ResponseConsumer r a where
    type ResponseMetadata a
    responseConsumer :: r -> IORef (ResponseMetadata a) -> HTTPResponseConsumer a

instance ResponseConsumer r (HTTP.Response L.ByteString) where
    type ResponseMetadata (HTTP.Response L.ByteString) = ()
    responseConsumer _ _ status headers bufsource = do
      chunks <- bufsource $$ CL.consume
      return (HTTP.Response status HTTP.http11 headers $ L.fromChunks chunks)

class (SignQuery r, ResponseConsumer r a, Monoid (ResponseMetadata a))
    => Transaction r a | r -> a, a -> r

data Credentials
    = Credentials {
        accessKeyID :: B.ByteString
      , secretAccessKey :: B.ByteString
      }
    deriving (Show)
             
credentialsDefaultFile :: IO FilePath
credentialsDefaultFile = (</> ".aws-keys") <$> getHomeDirectory

credentialsDefaultKey :: T.Text
credentialsDefaultKey = "default"

loadCredentialsFromFile :: FilePath -> T.Text -> IO (Maybe Credentials)
loadCredentialsFromFile file key = do
  contents <- map T.words . T.lines <$> T.readFile file
  return $ do 
    [_key, keyID, secret] <- find (hasKey key) contents
    return Credentials { accessKeyID = T.encodeUtf8 keyID, secretAccessKey = T.encodeUtf8 secret }
      where
        hasKey _ [] = False
        hasKey k (k2 : _) = k == k2

loadCredentialsFromEnv :: IO (Maybe Credentials)
loadCredentialsFromEnv = do
  env <- getEnvironment
  let lk = flip lookup env
      keyID = lk "AWS_ACCESS_KEY_ID"
      secret = lk "AWS_ACCESS_KEY_SECRET" `mplus` lk "AWS_SECRET_ACCESS_KEY"
  return (Credentials <$> (T.encodeUtf8 . T.pack <$> keyID) <*> (T.encodeUtf8 . T.pack <$> secret))
  
loadCredentialsFromEnvOrFile :: FilePath -> T.Text -> IO (Maybe Credentials)
loadCredentialsFromEnvOrFile file key = 
  do
    envcr <- loadCredentialsFromEnv
    case envcr of
      Just cr -> return (Just cr)
      Nothing -> loadCredentialsFromFile file key

loadCredentialsDefault :: IO (Maybe Credentials)
loadCredentialsDefault = do
  file <- credentialsDefaultFile
  loadCredentialsFromEnvOrFile file credentialsDefaultKey

data Protocol
    = HTTP
    | HTTPS
    deriving (Show)

defaultPort :: Protocol -> Int
defaultPort HTTP = 80
defaultPort HTTPS = 443

data Method
    = Get
    | PostQuery
    | Post
    | Put
    | Delete
    deriving (Show, Eq)

httpMethod :: Method -> HTTP.Method
httpMethod Get       = "GET"
httpMethod PostQuery = "POST"
httpMethod Post      = "POST"
httpMethod Put       = "PUT"
httpMethod Delete    = "DELETE"

data SignedQuery
    = SignedQuery {
        sqMethod :: Method
      , sqProtocol :: Protocol
      , sqHost :: B.ByteString
      , sqPort :: Int
      , sqPath :: B.ByteString
      , sqQuery :: HTTP.Query
      , sqDate :: Maybe UTCTime
      , sqAuthorization :: Maybe B.ByteString
      , sqContentType :: Maybe B.ByteString
      , sqContentMd5 :: Maybe B.ByteString
      , sqAmzHeaders :: HTTP.RequestHeaders
      , sqOtherHeaders :: HTTP.RequestHeaders
      , sqBody :: Maybe (HTTP.RequestBody (C.ResourceT IO))
      , sqStringToSign :: B.ByteString
      }
    --deriving (Show)

queryToHttpRequest :: SignedQuery -> HTTP.Request (C.ResourceT IO)
queryToHttpRequest SignedQuery{..}
    = HTTP.def {
        HTTP.method = httpMethod sqMethod
      , HTTP.secure = case sqProtocol of
                        HTTP -> False
                        HTTPS -> True
      , HTTP.host = sqHost
      , HTTP.port = sqPort
      , HTTP.path = sqPath
      , HTTP.queryString = HTTP.renderQuery False sqQuery
      , HTTP.requestHeaders = catMaybes [fmap (\d -> ("Date", fmtRfc822Time d)) sqDate
                                        , fmap (\c -> ("Content-Type", c)) contentType
                                        , fmap (\md5 -> ("Content-MD5", md5)) sqContentMd5
                                        , fmap (\auth -> ("Authorization", auth)) sqAuthorization]
                              ++ sqAmzHeaders
                              ++ sqOtherHeaders
      , HTTP.requestBody = case sqMethod of
                             PostQuery -> HTTP.RequestBodyLBS . Blaze.toLazyByteString $ HTTP.renderQueryBuilder False sqQuery
                             _         -> case sqBody of
                                            Nothing -> HTTP.RequestBodyBuilder 0 mempty
                                            Just x  -> x
      , HTTP.decompress = HTTP.alwaysDecompress
      , HTTP.checkStatus = \_ _ -> Nothing
      }
    where contentType = case sqMethod of
                           PostQuery -> Just "application/x-www-form-urlencoded; charset=utf-8"
                           _ -> sqContentType

queryToUri :: SignedQuery -> B.ByteString
queryToUri SignedQuery{..}
    = B.concat [
       case sqProtocol of
         HTTP -> "http://"
         HTTPS -> "https://"
      , sqHost
      , if sqPort == defaultPort sqProtocol then "" else T.encodeUtf8 . T.pack $ ':' : show sqPort
      , sqPath
      , HTTP.renderQuery True sqQuery
      ]

data TimeInfo
    = Timestamp
    | ExpiresAt { fromExpiresAt :: UTCTime }
    | ExpiresIn { fromExpiresIn :: NominalDiffTime }
    deriving (Show)

data AbsoluteTimeInfo
    = AbsoluteTimestamp { fromAbsoluteTimestamp :: UTCTime }
    | AbsoluteExpires { fromAbsoluteExpires :: UTCTime }
    deriving (Show)

fromAbsoluteTimeInfo :: AbsoluteTimeInfo -> UTCTime
fromAbsoluteTimeInfo (AbsoluteTimestamp time) = time
fromAbsoluteTimeInfo (AbsoluteExpires time) = time

makeAbsoluteTimeInfo :: TimeInfo -> UTCTime -> AbsoluteTimeInfo
makeAbsoluteTimeInfo Timestamp     now = AbsoluteTimestamp now
makeAbsoluteTimeInfo (ExpiresAt t) _   = AbsoluteExpires t
makeAbsoluteTimeInfo (ExpiresIn s) now = AbsoluteExpires $ addUTCTime s now

data SignatureData
    = SignatureData {
        signatureTimeInfo :: AbsoluteTimeInfo
      , signatureTime :: UTCTime
      , signatureCredentials :: Credentials
      }

signatureData :: TimeInfo -> Credentials -> IO SignatureData
signatureData rti cr = do
  now <- getCurrentTime
  let ti = makeAbsoluteTimeInfo rti now
  return SignatureData { signatureTimeInfo = ti, signatureTime = now, signatureCredentials = cr }

class SignQuery r where
    type Info r :: *
    signQuery :: r -> Info r -> SignatureData -> SignedQuery

data AuthorizationHash
    = HmacSHA1
    | HmacSHA256
    deriving (Show)

amzHash :: AuthorizationHash -> B.ByteString
amzHash HmacSHA1 = "HmacSHA1"
amzHash HmacSHA256 = "HmacSHA256"

signature :: Credentials -> AuthorizationHash -> B.ByteString -> B.ByteString
signature cr ah input = Base64.encode sig
    where
      sig = case ah of
              HmacSHA1 -> computeSig (undefined :: SHA1.SHA1)
              HmacSHA256 -> computeSig (undefined :: SHA256.SHA256)
      computeSig :: Crypto.Hash c d => d -> B.ByteString
      computeSig t = Serialize.encode (HMAC.hmac' key input `asTypeOf` t)
      key :: HMAC.MacKey c d
      key = HMAC.MacKey (secretAccessKey cr)

tryError :: (Exception e, C.MonadResource m, MonadBaseControl IO m) => m b -> m (Either e b)
tryError = Control.Exception.Lifted.try

queryList :: (a -> [(B.ByteString, B.ByteString)]) -> B.ByteString -> [a] -> [(B.ByteString, B.ByteString)]
queryList f prefix xs = concat $ zipWith combine prefixList (map f xs)
    where prefixList = map (dot prefix . BU.fromString . show) [(1 :: Int) ..]
          combine pf = map $ first (pf `dot`)
          dot x y = B.concat [x, BU.fromString ".", y]

awsBool :: Bool -> B.ByteString
awsBool True = "true"
awsBool False = "false"

awsTrue :: B.ByteString
awsTrue = awsBool True

awsFalse :: B.ByteString
awsFalse = awsBool False

fmtTime :: String -> UTCTime -> B.ByteString
fmtTime s t = BU.fromString $ formatTime defaultTimeLocale s t

fmtRfc822Time :: UTCTime -> B.ByteString
fmtRfc822Time = fmtTime "%a, %_d %b %Y %H:%M:%S GMT"

fmtAmzTime :: UTCTime -> B.ByteString
fmtAmzTime = fmtTime "%Y-%m-%dT%H:%M:%S"

fmtTimeEpochSeconds :: UTCTime -> B.ByteString
fmtTimeEpochSeconds = fmtTime "%s"

readHex2 :: [Char] -> Maybe Word8
readHex2 [c1,c2] = do n1 <- readHex1 c1
                      n2 <- readHex1 c2
                      return . fromIntegral $ n1 * 16 + n2
    where
      readHex1 c | c >= '0' && c <= '9' = Just $ ord c - ord '0'
                 | c >= 'A' && c <= 'F' = Just $ ord c - ord 'A' + 10
                 | c >= 'a' && c <= 'f' = Just $ ord c - ord 'a' + 10
      readHex1 _                        = Nothing
readHex2 _ = Nothing

-- XML
newtype XmlException = XmlException { xmlErrorMessage :: String }
    deriving (Show, Typeable)

instance E.Exception XmlException

elContent :: T.Text -> Cursor -> [T.Text]
elContent name = laxElement name &/ content

elCont :: T.Text -> Cursor -> [String]
elCont name = laxElement name &/ content &| T.unpack

force :: F.Failure XmlException m => String -> [a] -> m a
force = Cu.force . XmlException

forceM :: F.Failure XmlException m => String -> [m a] -> m a
forceM = Cu.forceM . XmlException

textReadInt :: (F.Failure XmlException m, Num a) => T.Text -> m a
textReadInt s = case reads $ T.unpack s of
                  [(n,"")] -> return $ fromInteger n
                  _        -> F.failure $ XmlException "Invalid Integer"

readInt :: (F.Failure XmlException m, Num a) => String -> m a
readInt s = case reads s of
              [(n,"")] -> return $ fromInteger n
              _        -> F.failure $ XmlException "Invalid Integer"

xmlCursorConsumer ::
    (Monoid m)
    => (Cu.Cursor -> Response m a)
    -> IORef m
    -> HTTPResponseConsumer a
xmlCursorConsumer parse metadataRef _status _headers source
    = do doc <- source $$ XML.sinkDoc XML.def
         let cursor = Cu.fromDocument doc
         let Response metadata x = parse cursor
         liftIO $ tellMetadataRef metadataRef metadata
         case x of
           Failure err -> liftIO $ C.monadThrow err
           Success v   -> return v
