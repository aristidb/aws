{-# LANGUAGE TypeFamilies, RecordWildCards, TupleSections, OverloadedStrings, MultiParamTypeClasses, FlexibleInstances, UndecidableInstances, GADTs, RankNTypes #-}
module Aws.S3.Commands.PutObject
where

import           Aws.Http
import           Aws.Response
import           Aws.S3.Info
import           Aws.S3.Metadata
import           Aws.S3.Model
import           Aws.S3.Query
import           Aws.Signature
import           Aws.Transaction
import           Control.Applicative
import           Control.Arrow              (second)
import           Data.ByteString.Char8      ({- IsString -})
import           Data.Maybe
import qualified Data.CaseInsensitive       as CI
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as T
import qualified Network.HTTP.Conduit       as HTTP
import qualified Network.HTTP.Types         as HTTP

data PutObject = PutObject {
  poObjectName :: T.Text,
  poBucket :: Bucket,
  poContentType :: Maybe T.Text,
  poCacheControl :: Maybe T.Text,
  poContentDisposition :: Maybe T.Text,
  poContentEncoding :: Maybe T.Text,
  poContentMD5 :: Maybe T.Text,
  poExpires :: Maybe Int,
  poAcl :: Maybe CannedAcl,
  poStorageClass :: Maybe StorageClass,
  poRequestBody  :: HTTP.RequestBody IO,
  poMetadata :: [(T.Text,T.Text)]
}

putObject :: T.Text -> Bucket -> HTTP.RequestBody IO -> PutObject
putObject obj bucket body = PutObject obj bucket Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing body []

data PutObjectResponse = PutObjectResponse{
  porVersionId :: Maybe T.Text
}

instance SignQuery PutObject where
    type Info PutObject = S3Info
    signQuery PutObject {..} = s3SignQuery S3Query {
                                 s3QMethod = Put
                               , s3QBucket = Just $ T.encodeUtf8 poBucket
                               , s3QSubresources = []
                               , s3QQuery = []
                               , s3QAmzHeaders = map (second T.encodeUtf8) $ catMaybes [
                                              ("Content-Type",) <$> poContentType
                                            , ("Expires",) <$> case poExpires of
                                                                 Just x -> Just $ T.pack $ show x
                                                                 Nothing -> Nothing
                                            , ("Cache-Control",) <$> poCacheControl
                                            , ("Content-Disposition",) <$> poContentDisposition
                                            , ("Content-Encoding",) <$> poContentEncoding
                                            , ("Content-MD5",) <$> poContentMD5
                                            , ("x-amz-acl",) <$> writeCannedAcl <$> poAcl
                                            , ("x-amz-storage-class",) <$> writeStorageClass <$> poStorageClass
                                            ] ++ map( \x -> (CI.mk . T.encodeUtf8 $ T.concat ["x-amz-meta-", fst x], snd x)) poMetadata
                               , s3QRequestBody = Just poRequestBody
                               , s3QObject = Just $ T.encodeUtf8 poObjectName
                               }

instance ResponseConsumer PutObject PutObjectResponse where
    type ResponseMetadata PutObjectResponse = S3Metadata
    responseConsumer _ _ _ _ _ = do return $ PutObjectResponse Nothing

instance Transaction PutObject PutObjectResponse

