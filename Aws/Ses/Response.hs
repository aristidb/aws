{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, OverloadedStrings, TypeFamilies #-}
module Aws.Ses.Response
where

import           Aws.Response
import           Aws.Ses.Error
import           Aws.Ses.Metadata
import           Aws.Xml
import           Data.IORef
import           Data.Maybe
import           Text.XML.Cursor            (($/), ($//))
import qualified Control.Failure            as F
import qualified Text.XML.Cursor            as Cu

sesResponseConsumer :: (Cu.Cursor -> Response SesMetadata a)
                    -> IORef SesMetadata
                    -> HTTPResponseConsumer a
sesResponseConsumer inner metadataRef status = xmlCursorConsumer parse metadataRef status
    where
      parse cursor = do
        let requestId' = listToMaybe $ cursor $// elContent "RequestID"
        tellMetadata $ SesMetadata requestId'
        case cursor $/ Cu.laxElement "Error" of
          []      -> inner cursor
          (err:_) -> fromError err

      fromError cursor = do
        errCode    <- force "Missing Error Code"    $ cursor $// elContent "Code"
        errMessage <- force "Missing Error Message" $ cursor $// elContent "Message"
        F.failure $ SesError status errCode errMessage
