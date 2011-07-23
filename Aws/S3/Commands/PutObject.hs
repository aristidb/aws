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
import qualified Network.HTTP.Enumerator    as HTTP
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

data PutObjectResponse = PutObjectResponse{
  porVersionId :: Maybe T.Text
}

instance SignQuery PutObject where
    type Info PutObject = S3Info
    signQuery PutObject {..} = s3SignQuery S3Query { 
                                 s3QMethod = Put
                               , s3QBucket = Just $ T.encodeUtf8 poBucket
                               , s3QSubresources = []
                               , s3QQuery = HTTP.simpleQueryToQuery $ map (second T.encodeUtf8) $ catMaybes [
                                              ("Content-Type",) <$> poContentType
                                            , ("Expires",) <$> case poExpires of
                                                                 Just x -> Just $ T.pack $ show x
                                                                 Nothing -> Nothing
                                            , ("Cache-Control",) <$> poCacheControl
                                            , ("Content-Disposition",) <$> poContentDisposition
                                            , ("Content-Encoding",) <$> poContentEncoding
                                            , ("Content-MD5",) <$> poContentMD5
                                            ]
                               , s3QAmzHeaders = catMaybes [
                                              (CI.mk $ T.encodeUtf8 "x-amz-acl",) <$> case poAcl of
                                                                                    Just x -> Just $ T.encodeUtf8 $ writeCannedAcl x
                                                                                    Nothing -> Nothing
                                            --, ("x-amz-storage-class",) <$> case poStorageClass of
                                            --                                 Just x -> Just $ writeStorageClass x
                                            --                                 Nothing -> Nothing
                                            ] -- ++ map( \x -> ( T.encodeUtf8 $ T.concat ["x-amz-meta-", fst x], snd x)) poMetadata 
                               , s3QRequestBody = Just poRequestBody
                               , s3QObject = Just $ T.encodeUtf8 poObjectName
                               }

instance ResponseIteratee PutObject PutObjectResponse where
    type ResponseMetadata PutObjectResponse = S3Metadata
    responseIteratee _ _ _ _ = do return $ PutObjectResponse Nothing

instance Transaction PutObject PutObjectResponse

