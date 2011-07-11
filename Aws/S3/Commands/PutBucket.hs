{-# LANGUAGE RecordWildCards, TypeFamilies, OverloadedStrings, MultiParamTypeClasses, FlexibleInstances #-}
module Aws.S3.Commands.PutBucket where

import           Aws.Http
import           Aws.Response
import           Aws.S3.Info
import           Aws.S3.Metadata
import           Aws.S3.Model
import           Aws.S3.Query
import           Aws.S3.Response
import           Aws.Signature
import           Aws.Transaction
import           Control.Monad
import qualified Data.Text                    as T
import qualified Data.Text.Encoding           as T
import qualified Network.HTTP.Enumerator      as HTTPE
import qualified Text.XML.Enumerator.Resolved as XML

data PutBucket
    = PutBucket {
        pbBucket :: Bucket
      , pbCannedAcl :: Maybe CannedAcl
      , pbLocationConstraint :: LocationConstraint
      }
    deriving (Show)

data PutBucketResponse
    = PutBucketResponse
    deriving (Show)

instance SignQuery PutBucket where
    type Info PutBucket = S3Info

    signQuery PutBucket{..} = s3SignQuery (S3Query {
                                             s3QMethod       = Put
                                           , s3QBucket       = Just $ T.encodeUtf8 pbBucket
                                           , s3QSubresources = []
                                           , s3QQuery        = []
                                           , s3QAmzHeaders   = case pbCannedAcl of 
                                                                 Nothing -> []
                                                                 Just acl -> [("x-amz-acl", T.encodeUtf8 $ writeCannedAcl acl)]
                                           , s3QRequestBody
                                               = guard (not . T.null $ pbLocationConstraint) >>
                                                 (Just . HTTPE.RequestBodyLBS . XML.renderLBS) 
                                                 XML.Document {
                                                          XML.documentPrologue = XML.Prologue [] Nothing []
                                                        , XML.documentRoot = root
                                                        , XML.documentEpilogue = []
                                                        }
                                           })
        where root = XML.Element {
                               XML.elementName = "{http://s3.amazonaws.com/doc/2006-03-01/}CreateBucketConfiguration"
                             , XML.elementAttributes = []
                             , XML.elementNodes = [
                                                   XML.NodeElement (XML.Element {
                                                                             XML.elementName = "{http://s3.amazonaws.com/doc/2006-03-01/}LocationConstraint"
                                                                           , XML.elementAttributes = []
                                                                           , XML.elementNodes = [XML.NodeContent pbLocationConstraint]
                                                                           })
                                                  ]
                             }

instance ResponseIteratee r PutBucketResponse where
    type ResponseMetadata PutBucketResponse = S3Metadata
    
    responseIteratee _ = s3ResponseIteratee inner
        where inner _status _headers = return PutBucketResponse

instance Transaction PutBucket PutBucketResponse
