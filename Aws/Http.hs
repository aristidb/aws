{-# LANGUAGE RecordWildCards #-}

module Aws.Http
where
  
import           Aws.Util
import           Control.Applicative
import           Control.Monad
import           Data.IORef
import           Data.Maybe
import           Data.Time
import           Data.Time.Clock.POSIX
import           Network.Curl
import           Network.URI
import qualified Data.ByteString        as B
import qualified Data.ByteString.Lazy   as L
import qualified Data.ByteString.Unsafe as BU
import qualified Foreign.Marshal.Array  as FMA
  
data Protocol
    = HTTP
    | HTTPS
    deriving (Show)

defaultPort :: Protocol -> Int
defaultPort HTTP = 80
defaultPort HTTPS = 443

data Method
    = GET
    | POST
    deriving (Show, Eq)

-- Note/TODO: Large data: just use files
  
data HttpRequest
    = HttpRequest {
        requestMethod :: Method
      , requestDate :: Maybe UTCTime
      , requestUri :: URI
      , requestPostQuery :: [String]
      , requestBody :: L.ByteString
      }
    deriving (Show)

data HttpResponse
    = HttpResponse {
        responseError :: Maybe HttpError
      , responseStatus :: Int
      , responseBody :: L.ByteString
      }
    deriving (Show)

data HttpError
    = CurlError CurlCode
    | OtherError String
    deriving (Show)

curlRequest :: [CurlOption] -> HttpRequest -> IO HttpResponse
curlRequest otherOptions HttpRequest{..} = parse <$> curlGetResponse_ uriString options
    where uriString = show requestUri
          options = (case requestMethod of
                      GET -> [CurlHttpGet True]
                      POST -> [CurlPostFields requestPostQuery]) ++
                    (case requestDate of
                       Just d -> [CurlTimeValue . round . utcTimeToPOSIXSeconds $ d]
                       Nothing -> []) ++
                    [
                      CurlHttpHeaders headers
                    , CurlFailOnError False
                    ]
                    ++ otherOptions
          headers = case requestDate of
                      Just d -> ["Date: " ++ fmtRfc822Time d]
                      Nothing -> []
          parse :: CurlResponse_ [(String, String)] L.ByteString -> HttpResponse
          parse CurlResponse{..} = HttpResponse {
                                          responseError = CurlError respCurlCode <$ guard (respCurlCode /= CurlOK)
                                        , responseStatus = respStatus
                                        , responseBody = respBody
                                        }

curlGatherBSL :: IORef L.ByteString -> WriteFunction
curlGatherBSL r = gatherOutput_ $ \s -> do
                    bs <- L.fromChunks . return <$> B.packCStringLen s
                    modifyIORef r (`L.append` bs)

curlCallbackWriteBS :: (B.ByteString -> IO ()) -> WriteFunction
curlCallbackWriteBS f = gatherOutput_ (B.packCStringLen >=> f)

curlCallbackWriteBSL :: (L.ByteString -> IO ()) -> WriteFunction
curlCallbackWriteBSL f = curlCallbackWriteBS (f . L.fromChunks . return)

curlCallbackReadBS :: IO (Maybe [B.ByteString]) -> IO ReadFunction
curlCallbackReadBS next = do
  chunks <- newIORef Nothing
  return $ \ptr width count _ -> do
      let sz = fromIntegral $ width * count
      update chunks
      r <- readIORef chunks
      case r of
        Nothing       -> return Nothing
        Just []       -> return $ Just 0
        Just (x : xs) -> let (a, b) = B.splitAt sz x
                             l = B.length a
                         in do
                           BU.unsafeUseAsCString a (\src -> FMA.copyArray ptr src l)
                           writeIORef chunks (Just $ cons' b xs)
                           return (Just $ fromIntegral l)
    where
      update :: IORef (Maybe [B.ByteString]) -> IO ()
      update chunks = do
        r <- normalise <$> readIORef chunks
        when (isNothing r) $ writeIORef chunks =<< next

      cons' :: B.ByteString -> [B.ByteString] -> [B.ByteString]
      cons' x xs | B.null x  = xs
                 | otherwise = x : xs

      normalise :: Maybe [B.ByteString] -> Maybe [B.ByteString]
      normalise Nothing        = Nothing
      normalise (Just [])      = Nothing
      normalise x@(Just (_:_)) = x

curlCallbackReadBSL :: IO (Maybe [L.ByteString]) -> IO ReadFunction
curlCallbackReadBSL f = curlCallbackReadBS $ concatMap L.toChunks .: f

fixedReader :: [a] -> IO (IO (Maybe [a]))
fixedReader xs = do
  r <- newIORef xs
  return $ do
    d <- readIORef r
    writeIORef r []
    return $ Just d

curlCallbackReadBSFixed :: [B.ByteString] -> IO ReadFunction
curlCallbackReadBSFixed = curlCallbackReadBS <=< fixedReader

curlCallbackReadBSLFixed :: [L.ByteString] -> IO ReadFunction
curlCallbackReadBSLFixed = curlCallbackReadBSL <=< fixedReader
