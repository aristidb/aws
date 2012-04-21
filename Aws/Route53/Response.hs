{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, OverloadedStrings, TypeFamilies #-}
module Aws.Route53.Response
where

import           Aws.Response
import           Aws.Route53.Error
import           Aws.Route53.Metadata
import           Aws.Xml
import           Data.IORef
import           Data.Maybe
import           Text.XML.Cursor            (($/), ($//))
import qualified Control.Failure            as F
import qualified Text.XML.Cursor            as Cu

sesResponseConsumer :: (Cu.Cursor -> Response Route53Metadata a)
                    -> IORef Route53Metadata
                    -> HTTPResponseConsumer a
sesResponseConsumer inner metadataRef status = xmlCursorConsumer parse metadataRef status
    where
      parse cursor = do
        let requestId' = listToMaybe $ cursor $// elContent "RequestID"
        tellMetadata $ Route53Metadata requestId'
        case cursor $/ Cu.laxElement "Error" of
          []      -> inner cursor
          (err:_) -> fromError err

      fromError cursor = do
        errCode    <- force "Missing Error Code"    $ cursor $// elContent "Code"
        errMessage <- force "Missing Error Message" $ cursor $// elContent "Message"
        F.failure $ Route53Error status errCode errMessage
