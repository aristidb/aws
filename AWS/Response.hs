{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module AWS.Response
where
  
import           AWS.Http
import           Control.Applicative
import           Control.Monad
import           Control.Shortcircuit
import           Data.Char
import           Data.Function
import           Text.XML.Light
import qualified Data.ByteString.Lazy.UTF8 as BLU

data Response
    = Response {
        httpResponse :: HttpResponse
      }
    deriving (Show)

class FromResponse a where
    fromResponse :: Response -> ParseResult a

data ParseError
    = InvalidXml
    | XmlElementNotFound String
    | UnexpectedElementName String String
    deriving (Show)

newtype ParseResult a = ParseResult { runParseResult :: Either ParseError a }
    deriving (Show, Functor)

instance Monad ParseResult where
    return = ParseResult . Right
    m >>= k = ParseResult $ runParseResult m `bind` (runParseResult . k)
        where
          Right m `bind` k = k m
          Left e `bind` _ = Left e

instance Applicative ParseResult where
    pure = return
    (<*>) = ap

instance Shortcircuit (ParseResult a) where
    isTrue a = isTrue $ runParseResult a

maybeRaise :: ParseError -> Maybe a -> ParseResult a
maybeRaise e = ParseResult . maybe (Left e) Right

parseResultToMaybe :: ParseResult a -> Maybe a
parseResultToMaybe = either (const Nothing) Just . runParseResult

parseXmlResponse :: HttpResponse -> ParseResult Element
parseXmlResponse = maybeRaise InvalidXml . parseXMLDoc . BLU.toString . responseBody

findElementName :: String -> Element -> ParseResult Element
findElementName name = maybeRaise (XmlElementNotFound name) . filterElementName ((`strEqI` name) . qName)

testElementName :: String -> Element -> ParseResult ()
testElementName expectedName el
    = ParseResult $ case expectedName `strEqI` actualName of 
                      True -> Right ()
                      False -> Left (UnexpectedElementName expectedName actualName)
    where actualName = qName (elName el)

strEqI :: String -> String -> Bool
strEqI = (==) `on` map toLower
