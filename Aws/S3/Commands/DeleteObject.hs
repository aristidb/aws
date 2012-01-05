{-# LANGUAGE TypeFamilies, RecordWildCards, TupleSections, OverloadedStrings, MultiParamTypeClasses, FlexibleInstances, UndecidableInstances, GADTs, RankNTypes #-}
module Aws.S3.Commands.DeleteObject
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
import           Data.ByteString.Char8      ({- IsString -})
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as T

data DeleteObject = DeleteObject {
  doObjectName :: T.Text,
  doBucket :: Bucket
}

data DeleteObjectResponse = DeleteObjectResponse{
}

instance SignQuery DeleteObject where
    type Info DeleteObject = S3Info
    signQuery DeleteObject {..} = s3SignQuery S3Query {
                                 s3QMethod = Delete
                               , s3QBucket = Just $ T.encodeUtf8 doBucket
                               , s3QSubresources = []
                               , s3QQuery = []
                               , s3QContentType = Nothing
                               , s3QContentMd5 = Nothing
                               , s3QAmzHeaders = []
                               , s3QRequestBody = Nothing
                               , s3QObject = Just $ T.encodeUtf8 doObjectName
                               }

instance ResponseConsumer DeleteObject DeleteObjectResponse where
    type ResponseMetadata DeleteObjectResponse = S3Metadata
    responseConsumer _ = s3ResponseConsumer $ \_ _ _ ->
                         return DeleteObjectResponse


instance Transaction DeleteObject DeleteObjectResponse

