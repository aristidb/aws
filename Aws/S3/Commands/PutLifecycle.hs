{-# LANGUAGE RecordWildCards, TypeFamilies, OverloadedStrings, MultiParamTypeClasses #-}
module Aws.S3.Commands.PutLifecycle
where

import           Aws.Core
import           Aws.S3.Core
import           Crypto.Classes
import qualified Crypto.Hash.MD5       as MD5
import           Data.ByteString.Char8 ({- IsString -})
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy  as L
import qualified Data.Map              as M
import qualified Data.Text             as T
import qualified Data.Text.Encoding    as T
import qualified Network.HTTP.Conduit  as HTTP
import qualified Network.HTTP.Types    as HTTP
import qualified Text.XML              as XML

-- http://docs.amazonwebservices.com/AmazonS3/latest/API/RESTBucketPUTlifecycle.html

data PutLifecycle
    = PutLifecycle {
        plBucket :: Bucket
      , plRules  :: [LifecycleRule]
      }
    deriving (Show)

data LifecycleRule
    = LifecycleRule {
        lrRuleId  :: T.Text
      , lrPrefix  :: T.Text
      , lrEnabled :: Bool
      , lrDays    :: Int
      }
  deriving (Show)

data PutLifecycleResponse
    = PutLifecycleResponse
    deriving (Show)

-- | ServiceConfiguration: 'S3Configuration'
instance SignQuery PutLifecycle where
    type ServiceConfiguration PutLifecycle = S3Configuration
    signQuery PutLifecycle {..} = s3SignQuery S3Query {
                                    s3QMethod = Put
                                  , s3QBucket = Just $ T.encodeUtf8 plBucket
                                  , s3QObject = Nothing
                                  , s3QSubresources = HTTP.toQuery [ ( "lifecycle" :: B8.ByteString
                                                                     , Nothing :: Maybe B8.ByteString ) ]
                                  , s3QQuery = []
                                  , s3QContentType = Nothing
                                  , s3QContentMd5 = Just $ hash body
                                  , s3QAmzHeaders = []
                                  , s3QOtherHeaders = []
                                  , s3QRequestBody = Just . HTTP.RequestBodyLBS $ body
                                  }

        where
          body = XML.renderLBS XML.def $ XML.Document {
                                  XML.documentPrologue = XML.Prologue [] Nothing []
                                , XML.documentRoot = root
                                , XML.documentEpilogue = []
                                }

          root = XML.Element { XML.elementName = "{http://s3.amazonaws.com/doc/2006-03-01/}LifecycleConfiguration"
                             , XML.elementAttributes = M.empty
                             , XML.elementNodes = map ruleDef plRules
                             }

          node name elements =
              XML.NodeElement XML.Element {
                                  XML.elementName = name
                                , XML.elementAttributes = M.empty
                                , XML.elementNodes = elements
                                }

          ruleDef LifecycleRule {..} =
              node "Rule" [ node "ID" [ XML.NodeContent lrRuleId ]
                          , node "Prefix" [ XML.NodeContent lrPrefix ]
                          , node "Status"
                            [ XML.NodeContent $ if lrEnabled then "Enabled" else "Disabled" ]
                          , node "Expiration"
                            [ node "Days" [ XML.NodeContent $ T.pack $ show lrDays ] ]
                          ]


instance ResponseConsumer PutLifecycle PutLifecycleResponse where
    type ResponseMetadata PutLifecycleResponse = S3Metadata

    responseConsumer _ = s3ResponseConsumer inner
        where inner _status _headers _source = return PutLifecycleResponse

instance Transaction PutLifecycle PutLifecycleResponse
