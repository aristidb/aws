module Aws.S3.Commands.DeleteObjects where

import           Aws.Core
import           Aws.S3.Core
import qualified Data.Map             as M
import           Data.Maybe
import qualified Data.Text            as T
import qualified Data.Text.Encoding   as T
import qualified Network.HTTP.Conduit as HTTP
import qualified Network.HTTP.Types   as HTTP
import qualified Text.XML             as XML
import qualified Text.XML.Cursor      as Cu
import           Text.XML.Cursor      (($/), (&|))
import           Crypto.Hash
import qualified Data.ByteString.Char8 as B
import           Data.ByteString.Char8 ({- IsString -})
import           Control.Applicative     ((<$>))

data DeleteObjects
    = DeleteObjects {
        dosBucket  :: Bucket
      , dosObjects :: [(Object, Maybe T.Text)] -- snd is an optional versionId
      , dosQuiet   :: Bool
      , dosMultiFactorAuthentication :: Maybe T.Text
      }
    deriving (Show)

-- simple use case: neither mfa, nor version specified, quiet
deleteObjects :: Bucket -> [T.Text] -> DeleteObjects
deleteObjects bucket objs =
    DeleteObjects {
            dosBucket  = bucket
          , dosObjects = zip objs $ repeat Nothing
          , dosQuiet   = True
          , dosMultiFactorAuthentication = Nothing
          }

data DeleteObjectsResponse
    = DeleteObjectsResponse {
        dorDeleted :: [DORDeleted]
      , dorErrors  :: [DORErrors]
      }
    deriving (Show)

--omitting DeleteMarker because it appears superfluous
data DORDeleted
    = DORDeleted {
        ddKey                   :: T.Text
      , ddVersionId             :: Maybe T.Text
      , ddDeleteMarkerVersionId :: Maybe T.Text
      }
    deriving (Show)

data DORErrors
    = DORErrors {
        deKey     :: T.Text
      , deCode    :: T.Text
      , deMessage :: T.Text
      }
    deriving (Show)

-- | ServiceConfiguration: 'S3Configuration'
instance SignQuery DeleteObjects where
    type ServiceConfiguration DeleteObjects = S3Configuration

    signQuery DeleteObjects {..} = s3SignQuery S3Query
      {
        s3QMethod       = Post
      , s3QBucket       = Just $ T.encodeUtf8 dosBucket
      , s3QSubresources = HTTP.toQuery [("delete" :: B.ByteString, Nothing :: Maybe B.ByteString)]
      , s3QQuery        = []
      , s3QContentType  = Nothing
      , s3QContentMd5   = Just $ hashlazy dosBody
      , s3QObject       = Nothing
      , s3QAmzHeaders   = maybeToList $ (("x-amz-mfa", ) . T.encodeUtf8) <$> dosMultiFactorAuthentication
      , s3QOtherHeaders = []
      , s3QRequestBody  = Just $ HTTP.RequestBodyLBS dosBody
      }
        where dosBody = XML.renderLBS XML.def XML.Document {
                    XML.documentPrologue = XML.Prologue [] Nothing []
                  , XML.documentRoot = root
                  , XML.documentEpilogue = []
                  }
              root = XML.Element {
                    XML.elementName = "Delete"
                  , XML.elementAttributes = M.empty
                  , XML.elementNodes = quietNode dosQuiet : (objectNode <$> dosObjects)
                  }
              objectNode (obj, mbVersion) = XML.NodeElement XML.Element {
                    XML.elementName = "Object"
                  , XML.elementAttributes = M.empty
                  , XML.elementNodes = keyNode obj : maybeToList (versionNode <$> mbVersion)
                  }
              versionNode = toNode "VersionId"
              keyNode     = toNode "Key"
              quietNode b = toNode "Quiet" $ if b then "true" else "false"
              toNode name content = XML.NodeElement XML.Element {
                    XML.elementName = name
                  , XML.elementAttributes = M.empty
                  , XML.elementNodes = [XML.NodeContent content]
                  }

instance ResponseConsumer DeleteObjects DeleteObjectsResponse where
    type ResponseMetadata DeleteObjectsResponse = S3Metadata

    responseConsumer _ _ = s3XmlResponseConsumer parse
        where parse cursor = do
                  dorDeleted <- sequence $ cursor $/ Cu.laxElement "Deleted" &| parseDeleted
                  dorErrors  <- sequence $ cursor $/ Cu.laxElement "Error" &| parseErrors
                  return DeleteObjectsResponse {..}
              parseDeleted c = do
                  ddKey <- force "Missing Key" $ c $/ elContent "Key"
                  let ddVersionId = listToMaybe $ c $/ elContent "VersionId"
                      ddDeleteMarkerVersionId = listToMaybe $ c $/ elContent "DeleteMarkerVersionId"
                  return DORDeleted {..}
              parseErrors c = do
                  deKey     <- force "Missing Key" $ c $/ elContent "Key"
                  deCode    <- force "Missing Code" $ c $/ elContent "Code"
                  deMessage <- force "Missing Message" $ c $/ elContent "Message"
                  return DORErrors {..}

instance Transaction DeleteObjects DeleteObjectsResponse

instance AsMemoryResponse DeleteObjectsResponse where
    type MemoryResponse DeleteObjectsResponse = DeleteObjectsResponse
    loadToMemory = return
