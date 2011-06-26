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
import qualified Data.ByteString.Base64     as Base64
import qualified Data.ByteString.UTF8       as BU
import qualified Data.Text                  as T
import qualified Text.XML.Enumerator.Cursor as Cu

data SdbResponse a
    = SdbResponse { 
        fromSdbResponse :: a
      , sdbResponseMetadata :: SdbMetadata
      }
    deriving (Show)

instance Functor SdbResponse where
    fmap f (SdbResponse a m) = SdbResponse (f a) m

instance (SdbFromResponse a) => ResponseIteratee (SdbResponse a) where
    type ResponseMetadata (SdbResponse a) = () -- TODO
    responseIteratee metadataRef status headers = xmlCursorIteratee parse status headers
        where parse cursor
                  = do let requestId' = listToMaybe $ cursor $// elCont "RequestID"
                       let boxUsage' = listToMaybe $ cursor $// elCont "BoxUsage"
                       let metadata = SdbMetadata requestId' boxUsage'
                       inner <- case parseInner cursor of
                                  Left err       -> Left $ putMetadata metadata err
                                  Right response -> return response
                       return $ SdbResponse inner metadata
              parseInner cursor = case cursor $/ Cu.laxElement "Error" of
                                    []      -> sdbFromResponse cursor
                                    (err:_) -> fromError err
              fromError cursor = do errCode <- sdbForce "Missing Error Code" $ cursor $// elCont "Code"
                                    errMessage <- sdbForce "Missing Error Message" $ cursor $// elCont "Message"
                                    Left $ SdbError status errCode errMessage ()

class SdbFromResponse a where
    sdbFromResponse :: Cu.Cursor -> Either (SdbError ()) a

sdbCheckResponseType :: a -> T.Text -> Cu.Cursor -> Either (SdbError ()) a
sdbCheckResponseType a n c = a <$ (sdbForce $ "Expected response type " ++ T.unpack n) (Cu.laxElement n c)

decodeBase64 :: Cu.Cursor -> Either (SdbError ()) String
decodeBase64 cursor =
  let encoded = T.unpack . T.concat $ cursor $/ Cu.content
      encoding = listToMaybe $ cursor $| Cu.laxAttribute "encoding" &| T.toCaseFold
  in
    case encoding of
      Nothing -> Right encoded
      Just "base64" -> handleErr . Base64.decode . BU.fromString $ encoded
          where handleErr = (flip SdbXmlError () . ("Invalid Base64 data: "++)) +++ BU.toString
      Just actual -> Left $ SdbXmlError ("Unrecognized encoding " ++ T.unpack actual) ()

sdbReadInt :: Num a => String -> Either (SdbError ()) a
sdbReadInt = readInt (SdbXmlError "Integer expected" ())
