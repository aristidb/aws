{-# LANGUAGE MultiParamTypeClasses #-}
module Aws.SimpleDb.Response
where

import           Aws.Http
import           Aws.Metadata
import           Aws.Response
import           Aws.SimpleDb.Error
import           Control.Applicative
import           Control.Monad.Compose.Class
import           Control.Monad.Reader.Class
import           Text.XML.Monad
import qualified Text.XML.Light              as XL

data SdbResponse a
    = SdbResponse { 
        fromSdbResponse :: a
      , sdbResponseMetadata :: SdbMetadata
      }
    deriving (Show)

instance Functor SdbResponse where
    fmap f (SdbResponse a m) = SdbResponse (f a) m

instance SdbFromResponse a => FromResponse (SdbResponse a) SdbError where
    fromResponse = do
          status <- asks (responseStatus . httpResponse)
          parseXmlResponse >>> fromXml status
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
                     errCode <- nameToErrorCode <$> strContent <<< findElementNameUI "Code"
                     errMessage <- strContent <<< findElementNameUI "Message"
                     raise $ SdbError status errCode errMessage NoMetadata

class SdbFromResponse a where
    sdbFromResponse :: Xml SdbError XL.Element a
