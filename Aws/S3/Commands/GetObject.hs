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
import           Control.Applicative
import           Control.Arrow         (second)
import           Data.ByteString.Char8 ({- IsString -})
import           Data.Maybe
import qualified Data.ByteString       as B
import qualified Data.Enumerator       as En
import qualified Data.Text             as T
import qualified Data.Text.Encoding    as T
import qualified Network.HTTP.Types    as HTTP

data GetObject a
    = GetObject {
        goBucket :: Bucket
      , goObjectName :: Object
      , goResponseIteratee :: HTTP.Status -> HTTP.ResponseHeaders -> En.Iteratee B.ByteString IO a
      , goResponseContentType :: Maybe T.Text
      , goResponseContentLanguage :: Maybe T.Text
      , goResponseExpires :: Maybe T.Text
      , goResponseCacheControl :: Maybe T.Text
      , goResponseContentDisposition :: Maybe T.Text
      , goResponseContentEncoding :: Maybe T.Text
      }

getObject :: Bucket -> T.Text -> (HTTP.Status -> HTTP.ResponseHeaders -> En.Iteratee B.ByteString IO a) -> GetObject a
getObject b o i = GetObject b o i Nothing Nothing Nothing Nothing Nothing Nothing

data GetObjectResponse a
    = GetObjectResponse a
    deriving (Show)

instance SignQuery (GetObject a) where
    type Info (GetObject a) = S3Info
    signQuery GetObject {..} = s3SignQuery S3Query { 
                                   s3QMethod = Get
                                 , s3QBucket = Just $ T.encodeUtf8 goBucket
                                 , s3QObject = Just $ T.encodeUtf8 goObjectName 
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
                                 }

instance ResponseIteratee (GetObject a) (GetObjectResponse a) where
    type ResponseMetadata (GetObjectResponse a) = S3Metadata
    responseIteratee GetObject{..} metadata status headers
        = GetObjectResponse <$> s3BinaryResponseIteratee (goResponseIteratee) metadata status headers

instance Transaction (GetObject a) (GetObjectResponse a)
