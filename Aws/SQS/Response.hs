{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts #-}
module Aws.SQS.Response
where

import           Aws.Metadata
import           Aws.Response
import           Aws.SQS.Error
import           Aws.SQS.Metadata
import           Control.Applicative
import           Control.Arrow               ((+++))
import           Control.Monad.Compose.Class
import           Data.Char
import           Text.XML.Monad
import qualified Data.ByteString.Base64      as Base64
import qualified Data.ByteString.UTF8        as BU
import qualified Text.XML.Light              as XL

data SqsResponse a
    = SqsResponse { 
        fromSdbResponse :: a
      , sqsResponseMetadata :: SqsMetadata
      }
    deriving (Show)

instance Functor SqsResponse where
    fmap f (SqsResponse a m) = SqsResponse (f a) m

instance (SqsFromResponse a) => ResponseIteratee (SqsResponse a) where
    --responseIteratee status headers = xmlResponseIteratee (fromXml <<< parseXmlResponse) status headers
    --    where fromXml :: SdbFromResponse a => Xml SdbError XL.Element (SdbResponse a)
    --          fromXml = do
    --                 requestId' <- strContent <<< findElementNameUI "RequestID"
    --                 boxUsage' <- tryMaybe $ strContent <<< findElementNameUI "BoxUsage"
    --                 let metadata = SdbMetadata requestId' boxUsage'
    --                 innerTry <- try $ fromXmlInner
    --                 inner <- case innerTry of
    --                   Left err -> raise $ setMetadata metadata err
    --                   Right response -> return response
    --                 return $ SdbResponse inner metadata
    --          fromXmlInner :: SdbFromResponse a => Xml SdbError XL.Element a
    --          fromXmlInner = do
    --                 xmlError <- tryMaybe $ findElementNameUI "Error"
    --                 case xmlError of
    --                   Just err -> mapply fromError err
    --                   Nothing -> sdbFromResponse
    --          fromError :: Xml SdbError XL.Element a
    --          fromError = do
    --                 errCode <- strContent <<< findElementNameUI "Code"
    --                 errMessage <- strContent <<< findElementNameUI "Message"
    --                 raise $ SdbError status errCode errMessage Nothing

class SqsFromResponse a where
    sqsFromResponse :: Xml SqsError XL.Element a

decodeBase64 :: Xml SqsError XL.Element String
decodeBase64 = do
  encoded <- strContent
  encoding <- tryMaybe $ findAttr (XL.unqual "encoding")
  raisesXml $ case map toLower <$> encoding of
                Nothing -> Right encoded
                Just "base64" -> (EncodingError . ("Invalid Base64 data: "++) +++ BU.toString) . Base64.decode . BU.fromString $ encoded
                Just actual -> Left $ UnexpectedAttributeValueQ actual "base64"
