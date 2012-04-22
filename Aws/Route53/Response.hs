{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, OverloadedStrings, TypeFamilies #-}
module Aws.Route53.Response
where

import           Aws.Response
import           Aws.Route53.Error
import           Aws.Route53.Metadata
import           Aws.Xml
import           Data.IORef
import           Data.List                  (find)
import           Data.Text                  (Text, unpack)
import           Data.Text.Encoding         (decodeUtf8)
import           Text.XML.Cursor            (($/), ($//))
import qualified Control.Failure            as F
import qualified Text.XML.Cursor            as Cu
import qualified Network.HTTP.Types as HTTP

-- TODO: the documentation seems to indicate that in case of errors the requestId is returned in the body
--       Have a look at Ses/Response.hs how to parse the requestId element. We may try both (header and
--       body element) on each response and sum the results with `mplus` in the Maybe monad.
--       http://docs.amazonwebservices.com/Route53/latest/DeveloperGuide/ResponseHeader_RequestID.html

route53ResponseConsumer :: (Cu.Cursor -> Response Route53Metadata a)
                        -> IORef Route53Metadata
                        -> HTTPResponseConsumer a
route53ResponseConsumer inner metadataRef status headers =
    xmlCursorConsumer parse metadataRef status headers
    where
      parse cursor = do
        tellMetadata . Route53Metadata . fmap decodeUtf8 $ findHeaderValue headers headerRequestId
        case cursor $/ Cu.laxElement "Error" of
          []      -> inner cursor
          (err:_) -> fromError err

      fromError cursor = do
        errCode    <- force "Missing Error Code"    $ cursor $// elContent "Code"
        errMessage <- force "Missing Error Message" $ cursor $// elContent "Message"
        F.failure $ Route53Error status errCode errMessage


route53CheckResponseType :: F.Failure XmlException m => a -> Text -> Cu.Cursor -> m a
route53CheckResponseType a n c = do 
    _ <- force ("Expected response type " ++ unpack n) (Cu.laxElement n c)
    return a

headerRequestId :: HTTP.Ascii -> HTTP.Header
headerRequestId = (,) "x-amzn-requestid"

findHeader :: [HTTP.Header] -> (HTTP.Ascii -> HTTP.Header) -> Maybe HTTP.Header
findHeader headers header = find (\h@(_,v) -> h == header v) headers

findHeaderValue :: [HTTP.Header] -> (HTTP.Ascii -> HTTP.Header) -> Maybe HTTP.Ascii
findHeaderValue headers = fmap snd . findHeader headers

