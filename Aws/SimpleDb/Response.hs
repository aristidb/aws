{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, OverloadedStrings, TypeFamilies #-}
module Aws.SimpleDb.Response
where

import           Aws.Response
import           Aws.SimpleDb.Error
import           Aws.SimpleDb.Metadata
import           Aws.Xml
import           Control.Applicative
import           Control.Arrow              ((+++), left)
import           Data.Maybe
import           Text.XML.Enumerator.Cursor (($|), ($/), ($//), (&|))
import qualified Control.Failure            as F
import qualified Data.ByteString.Base64     as Base64
import qualified Data.ByteString.UTF8       as BU
import qualified Data.Text                  as T
import qualified Text.XML.Enumerator.Cursor as Cu

sdbResponseIteratee inner metadataRef status headers = xmlCursorIteratee parse metadataRef status headers
    where parse cursor
              = do let requestId' = listToMaybe $ cursor $// elCont "RequestID"
                   let boxUsage' = listToMaybe $ cursor $// elCont "BoxUsage"
                   tellMetadata $ SdbMetadata requestId' boxUsage'
                   case cursor $/ Cu.laxElement "Error" of
                     []      -> inner cursor
                     (err:_) -> fromError err
          fromError cursor = do errCode <- force "Missing Error Code" $ cursor $// elCont "Code"
                                errMessage <- force "Missing Error Message" $ cursor $// elCont "Message"
                                F.failure $ SdbError status errCode errMessage

class SdbFromResponse a where
    sdbFromResponse :: Cu.Cursor -> Response SdbMetadata a

sdbCheckResponseType :: F.Failure XmlException m => a -> T.Text -> Cu.Cursor -> m a
sdbCheckResponseType a n c = do _ <- force ("Expected response type " ++ T.unpack n) (Cu.laxElement n c)
                                return a

decodeBase64 :: F.Failure XmlException m => Cu.Cursor -> m String
decodeBase64 cursor =
  let encoded = T.unpack . T.concat $ cursor $/ Cu.content
      encoding = listToMaybe $ cursor $| Cu.laxAttribute "encoding" &| T.toCaseFold
  in
    case encoding of
      Nothing -> return encoded
      Just "base64" -> case fmap BU.toString $ Base64.decode . BU.fromString $ encoded of
                         Left msg -> F.failure $ XmlException ("Invalid Base64 data: " ++ msg)
                         Right x -> return x
      Just actual -> F.failure $ XmlException ("Unrecognized encoding " ++ T.unpack actual)
