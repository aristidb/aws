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
import qualified Data.ByteString.Lazy      as L
import qualified Data.ByteString.Lazy.UTF8 as BLU

data Response
    = Response {
        httpResponse :: HttpResponse
      }
    deriving (Show)

class FromResponse a where
    fromResponse :: Response -> ParseResult a

data ParseError
    = EmptyDocument Int
    | InvalidXml
    | XmlElementNotFound String
    | UnexpectedElementName String String
    deriving (Show)

data ParseResult a
    = ParseOk { parseResult :: a }
    | ParseError { parseError :: ParseError }
    deriving (Show)

instance Functor ParseResult where
    fmap = liftM

instance Monad ParseResult where
    return = ParseOk
    ParseOk m    >>= k = k m
    ParseError e >>= _ = ParseError e

instance Applicative ParseResult where
    pure = return
    (<*>) = ap

instance Shortcircuit (ParseResult a) where
    isTrue (ParseOk _) = True
    isTrue (ParseError _) = False

maybeRaise :: ParseError -> Maybe a -> ParseResult a
maybeRaise e = maybe (ParseError e) ParseOk

parseResultToMaybe :: ParseResult a -> Maybe a
parseResultToMaybe (ParseOk a)    = Just a
parseResultToMaybe (ParseError _) = Nothing

parseXmlResponse :: HttpResponse -> ParseResult Element
parseXmlResponse resp = case responseBody resp of
                          body | L.null body -> ParseError . EmptyDocument $ responseStatus resp
                               | otherwise -> maybeRaise InvalidXml . parseXMLDoc . BLU.toString $ body

findElementName :: String -> Element -> ParseResult Element
findElementName name = maybeRaise (XmlElementNotFound name) . filterElementName ((`strEqI` name) . qName)

testElementName :: String -> Element -> ParseResult ()
testElementName expectedName el
    = case expectedName `strEqI` actualName of 
        True -> ParseOk ()
        False -> ParseError (UnexpectedElementName expectedName actualName)
    where actualName = qName (elName el)

strEqI :: String -> String -> Bool
strEqI = (==) `on` map toLower
