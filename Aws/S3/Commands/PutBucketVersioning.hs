module Aws.S3.Commands.PutBucketVersioning where

import           Aws.Core
import           Aws.S3.Core
import           Network.HTTP.Types (toQuery)
import qualified Data.Map             as M
import qualified Data.Text.Encoding   as T
import qualified Network.HTTP.Conduit as HTTP
import qualified Text.XML             as XML
import qualified Data.ByteString.Lazy.Char8 as B8

data VersioningState = VersioningSuspended | VersioningEnabled
    deriving (Show)

-- | Sets the versioning state of an existing bucket.
data PutBucketVersioning
    = PutBucketVersioning
      { pbvBucket :: Bucket
      , pbvVersioningConfiguration :: VersioningState
      }
    deriving (Show)

putBucketVersioning :: Bucket -> VersioningState -> PutBucketVersioning
putBucketVersioning = PutBucketVersioning

data PutBucketVersioningResponse
    = PutBucketVersioningResponse
    deriving (Show)

-- | ServiceConfiguration: 'S3Configuration'
instance SignQuery PutBucketVersioning where
    type ServiceConfiguration PutBucketVersioning = S3Configuration

    signQuery PutBucketVersioning{..} = s3SignQuery $ S3Query
      { s3QMethod       = Put
      , s3QBucket       = Just $ T.encodeUtf8 pbvBucket
      , s3QSubresources = toQuery [("versioning" :: B8.ByteString, Nothing :: Maybe B8.ByteString)]
      , s3QQuery        = []
      , s3QContentType  = Nothing
      , s3QContentMd5   = Nothing
      , s3QObject       = Nothing
      , s3QAmzHeaders   = []
      , s3QOtherHeaders = []
      , s3QRequestBody  = (Just . HTTP.RequestBodyLBS . XML.renderLBS XML.def)
         XML.Document
          { XML.documentPrologue = XML.Prologue [] Nothing []
          , XML.documentRoot = XML.Element
            { XML.elementName = "{http://s3.amazonaws.com/doc/2006-03-01/}VersioningConfiguration"
            , XML.elementAttributes = M.empty
            , XML.elementNodes = [ XML.NodeElement (XML.Element
              { XML.elementName = "{http://s3.amazonaws.com/doc/2006-03-01/}Status"
              , XML.elementAttributes = M.empty
              , XML.elementNodes = case pbvVersioningConfiguration of
                VersioningSuspended -> [XML.NodeContent "Suspended"]
                VersioningEnabled ->  [XML.NodeContent "Enabled"]
              })]
            }
          , XML.documentEpilogue = []
          }
      }

instance ResponseConsumer r PutBucketVersioningResponse where
    type ResponseMetadata PutBucketVersioningResponse = S3Metadata

    responseConsumer _ _ = s3ResponseConsumer $ \_ -> return PutBucketVersioningResponse

instance Transaction PutBucketVersioning PutBucketVersioningResponse

instance AsMemoryResponse PutBucketVersioningResponse where
    type MemoryResponse PutBucketVersioningResponse = PutBucketVersioningResponse
    loadToMemory = return
