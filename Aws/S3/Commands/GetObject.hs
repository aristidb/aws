{-# LANGUAGE TypeFamilies, RecordWildCards, TupleSections, OverloadedStrings, MultiParamTypeClasses, FlexibleInstances, UndecidableInstances, GADTs, RankNTypes #-}
module Aws.S3.Commands.GetObject
where

import           Aws.Http
import           Aws.Response
import           Aws.S3.Info
import           Aws.S3.Metadata
import           Aws.S3.Model
import           Aws.S3.Query
import           Aws.S3.Response
import           Aws.Signature
import           Aws.Transaction
import           Aws.Xml
import           Control.Applicative
import           Control.Arrow              (second)
import           Data.ByteString.Char8      ({- IsString -})
import           Data.Maybe
import qualified Data.ByteString            as B
import qualified Data.Enumerator            as En
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as T
import qualified Data.Traversable
import qualified Network.HTTP.Types         as HTTP
import qualified Text.XML.Enumerator.Cursor as Cu

data GetObject = GetObject {
  goObjectName :: T.Text,
  goBucket :: Bucket,
  goResponseContentType :: Maybe T.Text,
  goResponseContentLanguage :: Maybe T.Text,
  goResponseExpires :: Maybe T.Text,
  goResponseCacheControl :: Maybe T.Text,
  goResponseContentDisposition :: Maybe T.Text,
  goResponseContentEncoding :: Maybe T.Text,
  goResponseIteratee ::  (forall b. HTTP.Status -> HTTP.ResponseHeaders -> En.Iteratee B.ByteString IO b)
}

data GetObjectResponse = GetObjectResponse{
  gorDeleteMarker :: Maybe Bool,
  gorVersionId :: Maybe T.Text
}

instance SignQuery GetObject where
    type Info GetObject = S3Info
    signQuery GetObject {..} = s3SignQuery S3Query { 
                                 s3QMethod = Get
                               , s3QBucket = Just $ T.encodeUtf8 goBucket
                               , s3QSubresources = []
                               , s3QQuery = HTTP.simpleQueryToQuery $ map (second T.encodeUtf8) $ catMaybes [
                                              ("response-content-type",) <$> goResponseContentType
                                            , ("response-content-language",) <$> goResponseContentLanguage
                                            , ("response-expires",) <$> goResponseExpires
                                            , ("response-cache-control",) <$> goResponseCacheControl
                                            , ("response-content-disposition",) <$> goResponseContentDisposition
                                            , ("response-content-encoding",) <$> goResponseContentEncoding
                                            ]
                               , s3QAmzHeaders = []
                               , s3QRequestBody = Nothing
                               , s3QPath = Just $ T.encodeUtf8 goObjectName
                               }

instance ResponseIteratee GetObject GetObjectResponse where
    type ResponseMetadata GetObjectResponse = S3Metadata
    responseIteratee request metadata status headers = do
                                                          case request of
                                                            GetObject {..} -> s3BinaryResponseIteratee (goResponseIteratee) metadata status headers
                                                          return $ GetObjectResponse Nothing Nothing

instance Transaction GetObject GetObjectResponse
