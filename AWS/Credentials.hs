module AWS.Credentials
where
  
import AWS.Query
import AWS.Util
  
data Credentials
    = Credentials {
        accessKeyID :: String
      , secretAccessKey :: String
      }
    deriving (Show)

data TimeInfo
    = Timestamp { fromTimestamp :: UTCTime }
    | Expires { fromExpires :: UTCTime }
    deriving (Show)

addTimeInfo :: TimeInfo -> Query -> Query
addTimeInfo (Timestamp time) q@Query{..} = q {
                                             query = ("Timestamp", fmtAmzTime time) : query
                                           , date = Just time
                                           }
addTimeInfo (Expires time) q@Query{..} = q {
                                           query = ("Expires", fmtAmzTime time) : query
                                         }

addSignatureData :: Credentials -> Query -> Query
addSignatureData Credentials{..} q@Query{..} = q {
                                           query = [("AWSAccessKeyId", accessKeyID), ("SignatureMethod", "HmacSHA1"), ("SignatureVersion", "2")]
                                                   ++ query
                                         }
                                               
stringToSign Query{..} = case api of 
                           SimpleDB -> show method ++ "\n" ++
                                       host ++ "\n" ++
                                       path ++ "\n" ++
                                       HTTP.urlEncodeVars sortedQuery
    where sortedQuery = sortBy (compare `on` Utf8.encode . fst) query
                                               
signPreparedQuery :: Credentials -> Query -> Query
signPreparedQuery Credentials{..} q@Query{..} = q { query = ("Signature", sig) : query }
    where sig = Base64.encode $ hmac_sha1 (Utf8.encode secretAccessKey) (Utf8.encode . stringToSign $ q)
                           
signQuery :: TimeInfo -> Credentials -> Query -> Query
signQuery ti cr = signPreparedQuery cr . addSignatureData cr . addTimeInfo ti
