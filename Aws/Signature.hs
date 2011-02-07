{-# LANGUAGE RecordWildCards, OverloadedStrings #-}
module Aws.Signature
where
  
import           Aws.Credentials
import           Aws.Http
import           Aws.Query
import           Aws.Util
import           Control.Monad.IO.Class
import           Control.Monad.Trans.State
import           Data.List
import           Data.Maybe
import           Data.Time
import qualified Crypto.HMAC               as HMAC
import qualified Crypto.Hash.SHA1          as SHA1
import qualified Crypto.Hash.SHA256        as SHA256
import qualified Data.ByteString           as B
import qualified Data.ByteString.Base64    as Base64
import qualified Data.Serialize            as Serialize

data AbsoluteTimeInfo
    = AbsoluteTimestamp { fromAbsoluteTimestamp :: UTCTime }
    | AbsoluteExpires { fromAbsoluteExpires :: UTCTime }
    deriving (Show)

fromAbsoluteTimeInfo :: AbsoluteTimeInfo -> UTCTime
fromAbsoluteTimeInfo (AbsoluteTimestamp time) = time
fromAbsoluteTimeInfo (AbsoluteExpires time) = time

data TimeInfo
    = Timestamp
    | ExpiresAt { fromExpiresAt :: UTCTime }
    | ExpiresIn { fromExpiresIn :: NominalDiffTime }
    deriving (Show)

data AuthorizationHash
    = HmacSHA1
    | HmacSHA256
    deriving (Show)

amzHash :: AuthorizationHash -> B.ByteString
amzHash HmacSHA1 = "HmacSHA1"
amzHash HmacSHA256 = "HmacSHA256"

makeAbsoluteTimeInfo :: TimeInfo -> UTCTime -> AbsoluteTimeInfo
makeAbsoluteTimeInfo Timestamp     now = AbsoluteTimestamp now
makeAbsoluteTimeInfo (ExpiresAt t) _   = AbsoluteExpires t
makeAbsoluteTimeInfo (ExpiresIn s) now = AbsoluteExpires $ addUTCTime s now

calculateStringToSign :: AbsoluteTimeInfo -> Query -> B.ByteString
calculateStringToSign ti Query{..} 
    = case api of 
        SimpleDB -> B.intercalate "\n" [httpMethod method
                                       , host
                                       , path
                                       , urlEncodeVarsBS False sortedQuery]
        S3 -> B.intercalate "\n" $ concat [[httpMethod method]
                                          , [fromMaybe "" contentMd5]
                                          , [fromMaybe "" contentType]
                                          , [case ti of
                                               AbsoluteTimestamp time -> fmtRfc822Time time
                                               AbsoluteExpires time -> fmtTimeEpochSeconds time]
                                          , [] -- canonicalized AMZ headers
                                          , [canonicalizedResource]]
    where sortedQuery = sort query

signature :: Credentials -> AuthorizationHash -> B.ByteString -> B.ByteString
signature cr ah input = Base64.encode sig
    where
      sig = case ah of
              HmacSHA1 -> computeSig (undefined :: SHA1.SHA1)
              HmacSHA256 -> computeSig (undefined :: SHA256.SHA256)
      computeSig t = Serialize.encode (HMAC.hmac' key input `asTypeOf` t)
      key = HMAC.MacKey (secretAccessKey cr)

signQuery' :: AbsoluteTimeInfo -> UTCTime -> Credentials -> Query -> Query
signQuery' ti now cr query
    = flip execState query $ do
        modify $ \q -> q { date = Just now }
        let (authPrepare, authComplete) 
                = case (api', ti) of
                    (SimpleDB, _) -> authorizationQuery
                    (S3, AbsoluteTimestamp _) -> authorizationHeader
                    (S3, AbsoluteExpires _) -> authorizationQuery
                           
        authPrepare
        sts <- gets $ calculateStringToSign ti
        modify $ \q -> q { stringToSign = Just sts }
        authComplete $ signature cr ah sts
    where
      api' = api query

      ah = case api' of
             SimpleDB -> HmacSHA256
             S3 -> HmacSHA1
        
      addQueryItemM n v = modify $ addQueryItem n v
      
      authorizationQuery = (authorizationQueryPrepare, authorizationQueryComplete)
      authorizationQueryPrepare = do
        case ti of
          AbsoluteTimestamp time -> 
              addQueryItemM "Timestamp" $ fmtAmzTime time
          AbsoluteExpires time -> 
              addQueryItemM "Expires" $ case api' of
                                          SimpleDB -> fmtAmzTime time
                                          S3 -> fmtTimeEpochSeconds time
        addQueryItemM "AWSAccessKeyId" $ accessKeyID cr 
        addQueryItemM "SignatureMethod" $ amzHash ah
        case api' of
          SimpleDB -> addQueryItemM "SignatureVersion" "2"
          S3 -> return ()
      authorizationQueryComplete sig = do
        addQueryItemM "Signature" sig
      
      authorizationHeader = (authorizationHeaderPrepare, authorizationHeaderComplete)
      authorizationHeaderPrepare = return ()
      authorizationHeaderComplete sig = do
        modify $ \q -> q { authorization = Just $ B.concat [
                                            "AWS "
                                           , accessKeyID cr
                                           , ":"
                                           , sig
                                           ] }

signQuery :: MonadIO io => TimeInfo -> Credentials -> Query -> io Query
signQuery rti cr query = do
  now <- liftIO getCurrentTime
  let ti = makeAbsoluteTimeInfo rti now
  return $ signQuery' ti now cr query
