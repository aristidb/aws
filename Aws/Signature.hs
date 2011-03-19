{-# LANGUAGE TypeFamilies, OverloadedStrings #-}
module Aws.Signature
where
  
import           Aws.Credentials
import           Aws.Query
import           Data.Time
import qualified Crypto.Classes         as Crypto
import qualified Crypto.HMAC            as HMAC
import qualified Crypto.Hash.SHA1       as SHA1
import qualified Crypto.Hash.SHA256     as SHA256
import qualified Data.ByteString        as B
import qualified Data.ByteString.Base64 as Base64
import qualified Data.Serialize         as Serialize

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
      key = HMAC.MacKey (secretAccessKey cr)
