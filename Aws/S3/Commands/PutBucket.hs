module Aws.S3.Commands.PutBucket where

import           Aws.Core
import           Aws.S3.Core
import           Control.Monad
import           Data.Maybe
import qualified Data.Map             as M
import qualified Data.Text            as T
import qualified Data.Text.Encoding   as T
import qualified Network.HTTP.Conduit as HTTP
import qualified Text.XML             as XML

data PutBucket
    = PutBucket {
        pbBucket :: Bucket
      , pbCannedAcl :: Maybe CannedAcl
      , pbLocationConstraint :: LocationConstraint
      , pbXStorageClass :: Maybe StorageClass -- ^ Google Cloud Storage S3 nonstandard extension
      }
    deriving (Show)

putBucket :: Bucket -> PutBucket
putBucket bucket = PutBucket bucket Nothing locationUsClassic Nothing

data PutBucketResponse
    = PutBucketResponse
    deriving (Show)

-- | ServiceConfiguration: 'S3Configuration'
instance SignQuery PutBucket where
    type ServiceConfiguration PutBucket = S3Configuration

    signQuery PutBucket{..} = s3SignQuery (S3Query {
                                             s3QMethod       = Put
                                           , s3QBucket       = Just $ T.encodeUtf8 pbBucket
                                           , s3QSubresources = []
                                           , s3QQuery        = []
                                           , s3QContentType  = Nothing
                                           , s3QContentMd5   = Nothing
                                           , s3QObject       = Nothing
                                           , s3QAmzHeaders   = case pbCannedAcl of
                                                                 Nothing -> []
                                                                 Just acl -> [("x-amz-acl", T.encodeUtf8 $ writeCannedAcl acl)]
                                           , s3QOtherHeaders = []
                                           , s3QRequestBody
                                               = guard (not (null elts)) >>
                                                 (Just . HTTP.RequestBodyLBS . XML.renderLBS XML.def)
                                                 XML.Document {
                                                          XML.documentPrologue = XML.Prologue [] Nothing []
                                                        , XML.documentRoot = root
                                                        , XML.documentEpilogue = []
                                                        }
                                           })
        where root = XML.Element {
                               XML.elementName = "{http://s3.amazonaws.com/doc/2006-03-01/}CreateBucketConfiguration"
                             , XML.elementAttributes = M.empty
                             , XML.elementNodes = elts
                             }
              elts = catMaybes
                             [ if T.null pbLocationConstraint then Nothing else Just (locationconstraint pbLocationConstraint)
                             , fmap storageclass pbXStorageClass
                             ]
              locationconstraint c = XML.NodeElement (XML.Element {
                               XML.elementName = "{http://s3.amazonaws.com/doc/2006-03-01/}LocationConstraint"
                             , XML.elementAttributes = M.empty
                             , XML.elementNodes = [XML.NodeContent c]
                             })
              storageclass c = XML.NodeElement (XML.Element {
                               XML.elementName = "StorageClass"
                             , XML.elementAttributes = M.empty
                             , XML.elementNodes = [XML.NodeContent (writeStorageClass c)]
                             })

instance ResponseConsumer r PutBucketResponse where
    type ResponseMetadata PutBucketResponse = S3Metadata

    responseConsumer _ _ = s3ResponseConsumer $ \_ -> return PutBucketResponse

instance Transaction PutBucket PutBucketResponse

instance AsMemoryResponse PutBucketResponse where
    type MemoryResponse PutBucketResponse = PutBucketResponse
    loadToMemory = return
