{-# LANGUAGE TypeFamilies, RecordWildCards, TupleSections, OverloadedStrings, MultiParamTypeClasses, FlexibleInstances, UndecidableInstances, GADTs, RankNTypes #-}
module Aws.S3.Commands.GetObject
where

import           Aws.Core
import           Aws.S3.Core
import           Control.Applicative
import           Data.ByteString.Char8 ({- IsString -})
import qualified Data.ByteString.Char8 as B8
import qualified Data.Text             as T
import qualified Data.Text.Encoding    as T
import qualified Network.HTTP.Types    as HTTP

data GetObject a
    = GetObject {
        goBucket :: Bucket
      , goObjectName :: Object
      , goResponseConsumer :: HTTPResponseConsumer a
      , goVersionId :: Maybe T.Text
      , goResponseContentType :: Maybe T.Text
      , goResponseContentLanguage :: Maybe T.Text
      , goResponseExpires :: Maybe T.Text
      , goResponseCacheControl :: Maybe T.Text
      , goResponseContentDisposition :: Maybe T.Text
      , goResponseContentEncoding :: Maybe T.Text
      }

getObject :: Bucket -> T.Text -> HTTPResponseConsumer a -> GetObject a
getObject b o i = GetObject b o i Nothing Nothing Nothing Nothing Nothing Nothing Nothing

data GetObjectResponse a
    = GetObjectResponse ObjectMetadata a
    deriving (Show)

-- | ServiceConfiguration: 'S3Configuration'
instance SignQuery (GetObject a) where
    type ServiceConfiguration (GetObject a) = S3Configuration
    signQuery GetObject {..} = s3SignQuery S3Query {
                                   s3QMethod = Get
                                 , s3QBucket = Just $ T.encodeUtf8 goBucket
                                 , s3QObject = Just $ T.encodeUtf8 goObjectName
                                 , s3QSubresources = HTTP.toQuery [
                                                       ("versionId" :: B8.ByteString,) <$> goVersionId
                                                     ]
                                 , s3QQuery = HTTP.toQuery [
                                                ("response-content-type" :: B8.ByteString,) <$> goResponseContentType
                                              , ("response-content-language",) <$> goResponseContentLanguage
                                              , ("response-expires",) <$> goResponseExpires
                                              , ("response-cache-control",) <$> goResponseCacheControl
                                              , ("response-content-disposition",) <$> goResponseContentDisposition
                                              , ("response-content-encoding",) <$> goResponseContentEncoding
                                              ]
                                 , s3QContentType = Nothing
                                 , s3QContentMd5 = Nothing
                                 , s3QAmzHeaders = []
                                 , s3QOtherHeaders = []
                                 , s3QRequestBody = Nothing
                                 }

instance ResponseConsumer (GetObject a) (GetObjectResponse a) where
    type ResponseMetadata (GetObjectResponse a) = S3Metadata
    responseConsumer GetObject{..} metadata status headers source
        = do rsp <- s3BinaryResponseConsumer goResponseConsumer metadata status headers source
             om <- parseObjectMetadata headers
             return $ GetObjectResponse om rsp

instance Transaction (GetObject a) (GetObjectResponse a)
