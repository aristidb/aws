{-# LANGUAGE RecordWildCards, MultiParamTypeClasses, FunctionalDependencies #-}

module AWS.Query
where

import           AWS.Http
import           Control.Arrow
import           Control.Monad
import           Data.Time
import qualified Network.HTTP               as HTTP
import           Network.URI                (nullURI, URI(..), URIAuth(..))
import qualified Data.ByteString.Lazy.Char8 as L

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
      , body :: L.ByteString  
      }
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

dot :: String -> String -> String
dot x y = x ++ '.' : y

queryList :: (a -> [(String, String)]) -> String -> [a] -> [(String, String)]
queryList f prefix xs = concat $ zipWith combine prefixList (map f xs)
    where prefixList = map (dot prefix . show) [(1 :: Int) ..]
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

queryToRequest :: Query -> HttpRequest
queryToRequest Query{..}
    = HttpRequest { 
        requestMethod = method
      , requestUri = nullURI {
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
      , requestDate = date
      , requestPostQuery = guard isPost >> map urlEncodeSingleVar query
      , requestBody = L.empty
      }
    where isGet = method == HTTP.GET
          isPost = method == HTTP.POST
          urlEncodeSingleVar (a, b) = HTTP.urlEncode a ++ '=' : HTTP.urlEncode b
