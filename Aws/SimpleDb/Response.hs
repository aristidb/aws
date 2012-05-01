{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, OverloadedStrings, TypeFamilies #-}
module Aws.SimpleDb.Response
where

import           Aws.Core
import           Aws.SimpleDb.Error
import           Aws.SimpleDb.Metadata
import           Data.IORef
import           Data.Maybe
import           Text.XML.Cursor            (($|), ($/), ($//), (&|))
import qualified Control.Failure            as F
import qualified Data.ByteString.Base64     as Base64
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as T
import qualified Text.XML.Cursor            as Cu

sdbResponseConsumer :: (Cu.Cursor -> Response SdbMetadata a)
                    -> IORef SdbMetadata
                    -> HTTPResponseConsumer a
sdbResponseConsumer inner metadataRef status headers source
    = xmlCursorConsumer parse metadataRef status headers source
    where parse cursor
              = do let requestId' = listToMaybe $ cursor $// elContent "RequestID"
                   let boxUsage' = listToMaybe $ cursor $// elContent "BoxUsage"
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

decodeBase64 :: F.Failure XmlException m => Cu.Cursor -> m T.Text
decodeBase64 cursor =
  let encoded = T.concat $ cursor $/ Cu.content
      encoding = listToMaybe $ cursor $| Cu.laxAttribute "encoding" &| T.toCaseFold
  in
    case encoding of
      Nothing -> return encoded
      Just "base64" -> case Base64.decode . T.encodeUtf8 $ encoded of
                         Left msg -> F.failure $ XmlException ("Invalid Base64 data: " ++ msg)
                         Right x -> return $ T.decodeUtf8 x
      Just actual -> F.failure $ XmlException ("Unrecognized encoding " ++ T.unpack actual)
