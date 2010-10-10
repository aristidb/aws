{-# LANGUAGE RecordWildCards, MultiParamTypeClasses, FunctionalDependencies #-}

module AWS.Query
where

import           AWS.Credentials
import           AWS.HttpRequest
import           Control.Arrow
import           Control.Monad
import           Data.Function
import           Data.List
import           Data.Time
import qualified Network.HTTP               as HTTP
import           Network.URI                hiding (query)
import qualified Data.ByteString.Lazy.Char8 as L
import           Data.HMAC
import qualified Codec.Binary.Base64        as Base64
import qualified Codec.Binary.UTF8.String   as Utf8
import           System.Locale

class AsQuery d i | d -> i where
    asQuery :: i -> d -> Query
  
data API
    = SimpleDB
    deriving (Show)

data Query 
    = Query {
        api :: API
      , method :: HTTP.RequestMethod
      , protocol :: Protocol
      , host :: String
      , port :: Int
      , path :: String
      , query :: [(String, String)]
      , date :: Maybe UTCTime
      , metadata :: [HTTP.Header]
      , body :: L.ByteString  
      }
    deriving (Show)
      
data TimeInfo
    = Timestamp { fromTimestamp :: UTCTime }
    | Expires { fromExpires :: UTCTime }
    deriving (Show)
             
addQuery :: [(String, String)] -> Query -> Query
addQuery xs q = q { query = xs ++ query q }

addQueryIf :: Bool -> [(String, String)] -> Query -> Query
addQueryIf True  = addQuery
addQueryIf False = const id

addQueryUnless :: Bool -> [(String, String)] -> Query -> Query
addQueryUnless = addQueryIf . not
      
addQueryMaybe :: (a -> String) -> (String, Maybe a) -> Query -> Query
addQueryMaybe f (name, Just a) q = q { query = (name, f a) : query q }
addQueryMaybe _ (_, Nothing) q = q

dot x y = x ++ '.' : y

queryList :: (a -> [(String, String)]) -> String -> [a] -> [(String, String)]
queryList f prefix xs = concat $ zipWith combine prefixList (map f xs)
    where prefixList = map (dot prefix . show) [1..]
          combine pf = map $ first (pf `dot`)
          
addQueryList :: (a -> [(String, String)]) -> String -> [a] -> Query -> Query 
addQueryList f prefix xs = addQuery $ queryList f prefix xs
          
awsBool :: Bool -> String
awsBool True = "true"
awsBool False = "false"

awsTrue :: String
awsTrue = awsBool True

awsFalse :: String
awsFalse = awsBool False

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

queryToRequest :: Query -> HttpRequest
queryToRequest Query{..}
    = HttpRequest { 
        method = method
      , uri = nullURI {
                uriScheme = case protocol of
                              HTTP -> "http:"
                              HTTPS -> "https:"
              , uriAuthority = Just (URIAuth {
                                       uriUserInfo = ""
                                     , uriRegName = host
                                     , uriPort = if port == defaultPort protocol then "" else ':' : show port
                                     })
              , uriPath = path
              , uriQuery = guard isGet >> ('?' : HTTP.urlEncodeVars query)
              }
      , postQuery = guard isPost >> map urlEncodeSingleVar query
      , body = L.empty
      }
    where isGet = method == HTTP.GET
          isPost = method == HTTP.POST
          urlEncodeSingleVar (a, b) = HTTP.urlEncode a ++ '=' : HTTP.urlEncode b

fmtTime :: String -> UTCTime -> String
fmtTime = formatTime defaultTimeLocale

fmtRfc822Time :: UTCTime -> String
fmtRfc822Time = fmtTime "%a, %_d %b %Y %H:%M:%S GMT"

fmtAmzTime :: UTCTime -> String
fmtAmzTime = fmtTime "%Y-%m-%dT%H:%M:%S"
