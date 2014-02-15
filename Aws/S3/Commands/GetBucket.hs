module Aws.S3.Commands.GetBucket
where

import           Aws.Core
import           Aws.S3.Core
import           Control.Applicative
import           Data.ByteString.Char8 ({- IsString -})
import           Data.Maybe
import           Text.XML.Cursor       (($/), (&|), (&//))
import qualified Data.ByteString.Char8 as B8
import qualified Data.Text             as T
import qualified Data.Text.Encoding    as T
import qualified Data.Traversable
import qualified Network.HTTP.Types    as HTTP
import qualified Text.XML.Cursor       as Cu
import qualified Control.Monad.Trans.Resource as R

data GetBucket
    = GetBucket {
        gbBucket    :: Bucket
      , gbDelimiter :: Maybe T.Text
      , gbMarker    :: Maybe T.Text
      , gbMaxKeys   :: Maybe Int
      , gbPrefix    :: Maybe T.Text
      }
    deriving (Show)

getBucket :: Bucket -> GetBucket
getBucket bucket
    = GetBucket {
        gbBucket    = bucket
      , gbDelimiter = Nothing
      , gbMarker    = Nothing
      , gbMaxKeys   = Nothing
      , gbPrefix    = Nothing
      }

-- | Get the ObjectInfo for all objects in a GetBucket request.
-- 
-- This will paginate through the markers while the request is truncated.
-- 
-- This considers if a delimiter was set.
-- 
-- Note that this will take a long time for requests that match lots of objects.
-- 
-- Usage:
-- @
--     cfg <- Aws.baseConfiguration;
--     let s3cfg = Aws.defServiceConfig :: S3Configuration Aws.NormalQuery;
--     rsp <- withManager $ \mgr ->
--       getAllObjects (Aws.pureAws cfg s3cfg mgr) (getBucket name)
-- @
getAllObjects :: (GetBucket -> R.ResourceT IO GetBucketResponse)
                 -> GetBucket
                 -> R.ResourceT IO [ObjectInfo]
getAllObjects env r = fmap (concat . reverse) $ go r []
  where
    go req xs = do
      gbr :: GetBucketResponse <- env req
      -- nextMarker is set if a delimiter was used
      let next = maybe (objectKey (last (gbrContents gbr))) id (gbrNextMarker gbr)
      if gbrIsTruncated gbr == True
        then go (req { gbMarker = Just next }) ((gbrContents gbr) : xs)
        else return ((gbrContents gbr) : xs)

data GetBucketResponse
    = GetBucketResponse {
        gbrName           :: Bucket
      , gbrDelimiter      :: Maybe T.Text
      , gbrMarker         :: Maybe T.Text
      , gbrMaxKeys        :: Maybe Int
      , gbrPrefix         :: Maybe T.Text
      , gbrContents       :: [ObjectInfo]
      , gbrCommonPrefixes :: [T.Text]
      , gbrIsTruncated    :: Bool
      , gbrNextMarker     :: Maybe T.Text
      }
    deriving (Show)

-- | ServiceConfiguration: 'S3Configuration'
instance SignQuery GetBucket where
    type ServiceConfiguration GetBucket = S3Configuration
    signQuery GetBucket {..} = s3SignQuery S3Query {
                                 s3QMethod = Get
                               , s3QBucket = Just $ T.encodeUtf8 gbBucket
                               , s3QObject = Nothing
                               , s3QSubresources = []
                               , s3QQuery = HTTP.toQuery [
                                              ("delimiter" :: B8.ByteString ,) <$> gbDelimiter
                                            , ("marker",) <$> gbMarker
                                            , ("max-keys",) . T.pack . show <$> gbMaxKeys
                                            , ("prefix",) <$> gbPrefix
                                            ]
                               , s3QContentType = Nothing
                               , s3QContentMd5 = Nothing
                               , s3QAmzHeaders = []
                               , s3QOtherHeaders = []
                               , s3QRequestBody = Nothing
                               }

instance ResponseConsumer r GetBucketResponse where
    type ResponseMetadata GetBucketResponse = S3Metadata

    responseConsumer _ = s3XmlResponseConsumer parse
        where parse cursor
                  = do name <- force "Missing Name" $ cursor $/ elContent "Name"
                       let delimiter = listToMaybe $ cursor $/ elContent "Delimiter"
                       let marker = listToMaybe $ cursor $/ elContent "Marker"
                       let nextMarker = listToMaybe $ cursor $/ elContent "NextMarker"
                       maxKeys <- Data.Traversable.sequence . listToMaybe $ cursor $/ elContent "MaxKeys" &| textReadInt
                       let truncated = maybe True (/= "false") $ listToMaybe $ cursor $/ elContent "IsTruncated"
                       let prefix = listToMaybe $ cursor $/ elContent "Prefix"
                       contents <- sequence $ cursor $/ Cu.laxElement "Contents" &| parseObjectInfo
                       let commonPrefixes = cursor $/ Cu.laxElement "CommonPrefixes" &// Cu.content
                       return GetBucketResponse{
                                                gbrName           = name
                                              , gbrDelimiter      = delimiter
                                              , gbrMarker         = marker
                                              , gbrMaxKeys        = maxKeys
                                              , gbrPrefix         = prefix
                                              , gbrContents       = contents
                                              , gbrCommonPrefixes = commonPrefixes
                                              , gbrIsTruncated    = truncated
                                              , gbrNextMarker     = nextMarker
                                              }

instance Transaction GetBucket GetBucketResponse

instance AsMemoryResponse GetBucketResponse where
    type MemoryResponse GetBucketResponse = GetBucketResponse
    loadToMemory = return