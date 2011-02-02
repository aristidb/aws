{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts #-}
module Aws.SimpleDb.Response
where

import           Aws.Metadata
import           Aws.Response
import           Aws.SimpleDb.Error
import           Control.Applicative
import           Control.Arrow               ((+++))
import           Control.Monad.Compose.Class
import           Control.Monad.Reader.Class
import           Data.Char
import           Text.XML.Monad
import qualified Data.ByteString.Base64      as Base64
import qualified Data.ByteString.UTF8        as BU
import qualified Network.HTTP.Enumerator     as HTTP
import qualified Text.XML.Light              as XL

data SdbResponse a
    = SdbResponse { 
        fromSdbResponse :: a
      , sdbResponseMetadata :: SdbMetadata
      }
    deriving (Show)

instance Functor SdbResponse where
    fmap f (SdbResponse a m) = SdbResponse (f a) m

instance (SdbFromResponse a) => ResponseIteratee (SdbResponse a) where
    responseIteratee = xmlResponseIteratee $ do
          status <- asks HTTP.statusCode
          fromXml status <<< parseXmlResponse
        where fromXml :: SdbFromResponse a => Int -> Xml SdbError XL.Element (SdbResponse a)
              fromXml status = do
                     requestId' <- strContent <<< findElementNameUI "RequestID"
                     boxUsage' <- tryMaybe $ strContent <<< findElementNameUI "BoxUsage"
                     let metadata = SdbMetadata requestId' boxUsage'
                     innerTry <- try $ fromXmlInner status
                     inner <- case innerTry of
                       Left err -> raise $ setMetadata' metadata err
                       Right response -> return response
                     return $ SdbResponse inner metadata
              fromXmlInner :: SdbFromResponse a => Int -> Xml SdbError XL.Element a
              fromXmlInner status = do
                     xmlError <- tryMaybe $ findElementNameUI "Error"
                     case xmlError of
                       Just err -> mapply (fromError status) err
                       Nothing -> sdbFromResponse
              fromError :: Int -> Xml SdbError XL.Element a
              fromError status = do
                     errCode <- toErrorCode <$> strContent <<< findElementNameUI "Code"
                     errMessage <- strContent <<< findElementNameUI "Message"
                     raise $ SdbError status errCode errMessage NoMetadata

class SdbFromResponse a where
    sdbFromResponse :: Xml SdbError XL.Element a

decodeBase64 :: Xml SdbError XL.Element String
decodeBase64 = do
  encoded <- strContent
  encoding <- tryMaybe $ findAttr (XL.unqual "encoding")
  raisesXml $ case map toLower <$> encoding of
                Nothing -> Right encoded
                Just "base64" -> (EncodingError . ("Invalid Base64 data: "++) +++ BU.toString) . Base64.decode . BU.fromString $ encoded
                Just actual -> Left $ UnexpectedAttributeValueQ actual "base64"
