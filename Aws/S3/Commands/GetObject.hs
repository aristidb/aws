{-# LANGUAGE TypeFamilies, RecordWildCards, TupleSections, OverloadedStrings, MultiParamTypeClasses, FlexibleInstances, UndecidableInstances, GADTs, RankNTypes #-}
module Aws.S3.Commands.GetObject
where

import           Aws.Core
import           Aws.S3.Core
import           Control.Applicative
import           Control.Arrow         (second)
import           Data.ByteString.Char8 ({- IsString -})
import           Data.Maybe
import qualified Data.Text             as T
import qualified Data.Text.Encoding    as T
import qualified Network.HTTP.Types    as HTTP

data GetObject a
    = GetObject {
        goBucket :: Bucket
      , goObjectName :: Object
      , goResponseConsumer :: HTTPResponseConsumer a
      , goResponseContentType :: Maybe T.Text
      , goResponseContentLanguage :: Maybe T.Text
      , goResponseExpires :: Maybe T.Text
      , goResponseCacheControl :: Maybe T.Text
      , goResponseContentDisposition :: Maybe T.Text
      , goResponseContentEncoding :: Maybe T.Text
      }

getObject :: Bucket -> T.Text -> HTTPResponseConsumer a -> GetObject a
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
                                 , s3QContentType = Nothing
                                 , s3QContentMd5 = Nothing
                                 , s3QAmzHeaders = []
                                 , s3QOtherHeaders = []
                                 , s3QRequestBody = Nothing
                                 }

instance ResponseConsumer (GetObject a) (GetObjectResponse a) where
    type ResponseMetadata (GetObjectResponse a) = S3Metadata
    responseConsumer GetObject{..} metadata status headers source
        = GetObjectResponse <$> s3BinaryResponseConsumer goResponseConsumer metadata status headers source

instance Transaction (GetObject a) (GetObjectResponse a)
