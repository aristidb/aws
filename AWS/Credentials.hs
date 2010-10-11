{-# LANGUAGE RecordWildCards #-}

module AWS.Credentials
where
  
import           AWS.Query
import           AWS.Util
import           Control.Applicative
import           Data.Function
import           Data.List
import           Data.Time
import qualified Network.HTTP             as HTTP
import qualified Codec.Binary.UTF8.String as Utf8
import           Data.HMAC
import qualified Codec.Binary.Base64      as Base64

data Credentials
    = Credentials {
        accessKeyID :: String
      , secretAccessKey :: String
      }
    deriving (Show)

data AbsoluteTimeInfo
    = AbsoluteTimestamp { fromAbsoluteTimestamp :: UTCTime }
    | AbsoluteExpires { fromAbsoluteExpires :: UTCTime }
    deriving (Show)

data TimeInfo
    = Timestamp
    | ExpiresAt { fromExpiresAt :: UTCTime }
    | ExpiresIn { fromExpiresIn :: NominalDiffTime }
    deriving (Show)
      
makeAbsoluteTimeInfo :: TimeInfo -> IO AbsoluteTimeInfo
makeAbsoluteTimeInfo Timestamp     = AbsoluteTimestamp <$> getCurrentTime
makeAbsoluteTimeInfo (ExpiresAt t) = return $ AbsoluteExpires t
makeAbsoluteTimeInfo (ExpiresIn s) = AbsoluteExpires . addUTCTime s <$> getCurrentTime

addTimeInfo :: AbsoluteTimeInfo -> Query -> Query
addTimeInfo (AbsoluteTimestamp time) q@Query{..} 
    = q {
        query = ("Timestamp", fmtAmzTime time) : query
      , date = Just time
      }
addTimeInfo (AbsoluteExpires time) q@Query{..} 
    = q {
        query = ("Expires", fmtAmzTime time) : query
      }

addSignatureData :: Credentials -> Query -> Query
addSignatureData Credentials{..} q@Query{..} 
    = q {
        query = [("AWSAccessKeyId", accessKeyID), ("SignatureMethod", "HmacSHA1"), ("SignatureVersion", "2")]
                ++ query
      }

stringToSign :: Query -> String
stringToSign Query{..} 
    = case api of 
        SimpleDB -> show method ++ "\n" ++
                    host ++ "\n" ++
                    path ++ "\n" ++
                    HTTP.urlEncodeVars sortedQuery
    where sortedQuery = sortBy (compare `on` Utf8.encode . fst) query
                                               
signPreparedQuery :: Credentials -> Query -> Query
signPreparedQuery Credentials{..} q@Query{..} = q { query = ("Signature", sig) : query }
    where sig = Base64.encode $ hmac_sha1 (Utf8.encode secretAccessKey) (Utf8.encode . stringToSign $ q)
                           
signQuery :: AbsoluteTimeInfo -> Credentials -> Query -> Query
signQuery ti cr = signPreparedQuery cr . addSignatureData cr . addTimeInfo ti
