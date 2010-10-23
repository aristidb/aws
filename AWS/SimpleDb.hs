{-# LANGUAGE RecordWildCards, MultiParamTypeClasses, FlexibleInstances #-}

module AWS.SimpleDb
where
  
import           AWS.SimpleDb.Info
import           AWS.SimpleDb.Error
import           AWS.Query
import           AWS.Http
import           AWS.Response
import           AWS.Transaction
import           Control.Applicative
import           Data.Maybe
import qualified Network.HTTP               as HTTP
import qualified Data.ByteString.Lazy.Char8 as L
import           Data.Time
import           Data.Time.Clock.POSIX
import           Text.XML.Monad
import qualified Text.XML.Light             as XL
import           MonadLib
import           MonadLib.Compose

data SdbResponse a
    = SdbResponse { 
        fromSdbResponse :: a 
      , requestId :: String
      , boxUsage :: Maybe String
      }
    deriving (Show)

instance Functor SdbResponse where
    fmap f (SdbResponse a id bu) = SdbResponse (f a) id bu

instance SdbFromResponse a => FromResponse (SdbResponse a) Error where
    fromResponse = do
          status <- asks (responseStatus . httpResponse)
          parseXmlResponse >>> fromXml status
        where fromXml :: SdbFromResponse a => Int -> Xml Error XL.Element (SdbResponse a)
              fromXml status = do
                     requestId <- strContent <<< findElementNameUI "RequestID"
                     boxUsage <- tryMaybe $ strContent <<< findElementNameUI "BoxUsage"
                     innerTry <- try $ fromXmlInner status
                     inner <- case innerTry of
                       Left err -> raise (WithRequestId err requestId boxUsage)
                       Right response -> return response
                     return $ SdbResponse inner requestId boxUsage
              fromXmlInner :: SdbFromResponse a => Int -> Xml Error XL.Element a
              fromXmlInner status = do
                     xmlError <- tryMaybe $ findElementNameUI "Error"
                     case xmlError of
                       Just err -> mapply (fromError status) err
                       Nothing -> sdbFromResponse
              fromError :: Int -> Xml Error XL.Element a
              fromError status = do
                     errCode <- nameToErrorCode <$> strContent <<< findElementNameUI "Code"
                     errMessage <- strContent <<< findElementNameUI "Message"
                     raise $ SdbError status errCode errMessage

class SdbFromResponse a where
    sdbFromResponse :: Xml Error XL.Element a

data CreateDomain
    = CreateDomain {
        cdDomainName :: String
      }
    deriving (Show)

data CreateDomainResponse 
    = CreateDomainResponse
    deriving (Show)
             
createDomain :: String -> CreateDomain
createDomain name = CreateDomain { cdDomainName = name }
             
instance AsQuery CreateDomain SdbInfo where
    asQuery i CreateDomain{..} = addQuery [("Action", "CreateDomain"), ("DomainName", cdDomainName)] (sdbiBaseQuery i)

instance SdbFromResponse CreateDomainResponse where
    sdbFromResponse = CreateDomainResponse <$ testElementNameUI "CreateDomainResponse"

instance Transaction CreateDomain SdbInfo (SdbResponse CreateDomainResponse) Error
             
data DeleteDomain
    = DeleteDomain {
        ddDomainName :: String
      }
    deriving (Show)

data DeleteDomainResponse
    = DeleteDomainResponse
    deriving (Show)
             
deleteDomain :: String -> DeleteDomain
deleteDomain name = DeleteDomain { ddDomainName = name }
             
instance AsQuery DeleteDomain SdbInfo where
    asQuery i DeleteDomain{..} = addQuery [("Action", "DeleteDomain"), ("DomainName", ddDomainName)] (sdbiBaseQuery i)

instance SdbFromResponse DeleteDomainResponse where
    sdbFromResponse = DeleteDomainResponse <$ testElementNameUI "DeleteDomainResponse"
             
instance Transaction DeleteDomain SdbInfo (SdbResponse DeleteDomainResponse) Error

data ListDomains
    = ListDomains {
        ldMaxNumberOfDomains :: Maybe Int
      , ldNextToken :: Maybe String
      }
    deriving (Show)

data ListDomainsResponse 
    = ListDomainsResponse {
        ldrDomainNames :: [String]
      , ldrNextToken :: Maybe String
      }
    deriving (Show)

listDomains :: ListDomains
listDomains = ListDomains { ldMaxNumberOfDomains = Nothing, ldNextToken = Nothing }
             
instance AsQuery ListDomains SdbInfo where
    asQuery i ListDomains{..} = addQuery [("Action", "ListDomains")]
                                . addQueryMaybe show ("MaxNumberOfDomains", ldMaxNumberOfDomains)
                                . addQueryMaybe id ("NextToken", ldNextToken)
                                $ sdbiBaseQuery i

instance SdbFromResponse ListDomainsResponse where
    sdbFromResponse = do
      testElementNameUI "ListDomainsResponse"
      names <- inList strContent <<< findElementsNameUI "DomainName"
      nextToken <- tryMaybe $ strContent <<< findElementNameUI "NextToken"
      return $ ListDomainsResponse names nextToken

instance Transaction ListDomains SdbInfo (SdbResponse ListDomainsResponse) Error

data DomainMetadata
    = DomainMetadata {
        dmDomainName :: String
      }
    deriving (Show)

data DomainMetadataResponse
    = DomainMetadataResponse {
        dmrTimestamp :: UTCTime
      , dmrItemCount :: Integer
      , dmrAttributeValueCount :: Integer
      , dmrAttributeNameCount :: Integer
      , dmrItemNamesSizeBytes :: Integer
      , dmrAttributeValuesSizeBytes :: Integer
      , dmrAttributeNamesSizeBytes :: Integer
      }
    deriving (Show)
             
domainMetadata :: String -> DomainMetadata
domainMetadata name = DomainMetadata { dmDomainName = name }

instance AsQuery DomainMetadata SdbInfo where
    asQuery i DomainMetadata{..} = addQuery [("Action", "DomainMetadata"), ("DomainName", dmDomainName)] (sdbiBaseQuery i)

instance SdbFromResponse DomainMetadataResponse where
    sdbFromResponse = do
      testElementNameUI "DomainMetadataResponse"
      dmrTimestamp <- posixSecondsToUTCTime . fromInteger <$> readContent <<< findElementNameUI "Timestamp"
      dmrItemCount <- readContent <<< findElementNameUI "ItemCount"
      dmrAttributeValueCount <- readContent <<< findElementNameUI "AttributeValueCount"
      dmrAttributeNameCount <- readContent <<< findElementNameUI "AttributeNameCount"
      dmrItemNamesSizeBytes <- readContent <<< findElementNameUI "ItemNamesSizeBytes"
      dmrAttributeValuesSizeBytes <- readContent <<< findElementNameUI "AttributeValuesSizeBytes"
      dmrAttributeNamesSizeBytes <- readContent <<< findElementNameUI "AttributeNamesSizeBytes"
      return $ DomainMetadataResponse{..}

instance Transaction DomainMetadata SdbInfo (SdbResponse DomainMetadataResponse) Error

data Attribute a
    = ForAttribute { attributeName :: String, attributeData :: a }
    deriving (Show)
             
attributeQuery :: (a -> [(String, String)]) -> Attribute a -> [(String, String)]
attributeQuery  f (ForAttribute name x) =  ("Name", name) : f x

data GetAttributes
    = GetAttributes {
        gaItemName :: String
      , gaAttributeName :: Maybe String
      , gaConsistentRead :: Bool
      , gaDomainName :: String
      }
    deriving (Show)

data GetAttributesResponse
    = GetAttributesResponse {
        garAttributes :: [Attribute String]
      }
    deriving (Show)
             
getAttributes :: String -> String -> GetAttributes
getAttributes item domain = GetAttributes { gaItemName = item, gaAttributeName = Nothing, gaConsistentRead = False, gaDomainName = domain }

instance AsQuery GetAttributes SdbInfo where
    asQuery i GetAttributes{..}
        = addQuery [("Action", "GetAttributes"), ("ItemName", gaItemName), ("DomainName", gaDomainName)]
          . addQueryMaybe id ("AttributeName", gaAttributeName)
          . addQueryIf gaConsistentRead [("ConsistentRead", awsTrue)]
          $ sdbiBaseQuery i

instance SdbFromResponse GetAttributesResponse where
    sdbFromResponse = do
      testElementNameUI "GetAttributesResponse"
      attributes <- inList readAttribute <<< findElementsNameUI "Attribute"
      return $ GetAttributesResponse attributes
          where
            readAttribute = do
                        name <- strContent <<< findElementNameUI "Name"
                        value <- strContent <<< findElementNameUI "Value"
                        return $ ForAttribute name value

instance Transaction GetAttributes SdbInfo (SdbResponse GetAttributesResponse) Error

data PutAttributes
    = PutAttributes {
        paItemName :: String
      , paAttributes :: [Attribute SetAttribute]
      , paExpected :: [Attribute ExpectedAttribute]
      , paDomainName :: String
      }
    deriving (Show)

data PutAttributesResponse
    = PutAttributesResponse
    deriving (Show)
             
putAttributes :: String -> [Attribute SetAttribute] -> String -> PutAttributes
putAttributes item attributes domain = PutAttributes { 
                                         paItemName = item
                                       , paAttributes = attributes
                                       , paExpected = []
                                       , paDomainName = domain 
                                       }
                                       
instance AsQuery PutAttributes SdbInfo where
    asQuery i PutAttributes{..}
        = addQuery [("Action", "PutAttributes"), ("ItemName", paItemName), ("DomainName", paDomainName)]
          . addQueryList (attributeQuery setAttributeQuery) "Attribute" paAttributes
          . addQueryList (attributeQuery expectedAttributeQuery) "Expected" paExpected
          $ sdbiBaseQuery i

instance SdbFromResponse PutAttributesResponse where
    sdbFromResponse = PutAttributesResponse <$ testElementNameUI "PutAttributesResponse"

instance Transaction PutAttributes SdbInfo (SdbResponse PutAttributesResponse) Error

data SetAttribute
    = SetAttribute { setAttribute :: String, isReplaceAttribute :: Bool }
    deriving (Show)
             
addAttribute :: String -> String -> Attribute SetAttribute
addAttribute name value = ForAttribute name (SetAttribute value False)

replaceAttribute :: String -> String -> Attribute SetAttribute
replaceAttribute name value = ForAttribute name (SetAttribute value True)
             
setAttributeQuery :: SetAttribute -> [(String, String)]
setAttributeQuery (SetAttribute value replace)
    = ("Value", value) : (if replace then [("Replace", awsTrue)] else [])
             
data ExpectedAttribute
    = ExpectedValue { expectedAttributeValue :: String }
    | ExpectedExists { expectedAttributeExists :: Bool }
    deriving (Show)
             
expectedValue :: String -> String -> Attribute ExpectedAttribute
expectedValue name value = ForAttribute name (ExpectedValue value)

expectedExists :: String -> Bool -> Attribute ExpectedAttribute
expectedExists name exists = ForAttribute name (ExpectedExists exists)
             
expectedAttributeQuery :: ExpectedAttribute -> [(String, String)]
expectedAttributeQuery (ExpectedValue value) = [("Value", value)]
expectedAttributeQuery (ExpectedExists exists) = [("Exists", awsBool exists)]

data BatchPutAttributes
    = BatchPutAttributes {
        bpaItems :: [Item [Attribute SetAttribute]]
      , bpaDomainName :: String
      }
    deriving (Show)

data BatchPutAttributesResponse
    = BatchPutAttributesResponse
    deriving (Show)
             
batchPutAttributes :: [Item [Attribute SetAttribute]] -> String -> BatchPutAttributes
batchPutAttributes items domain = BatchPutAttributes { bpaItems = items, bpaDomainName = domain }

instance AsQuery BatchPutAttributes SdbInfo where
    asQuery i BatchPutAttributes{..}
        = addQuery [("Action", "BatchPutAttributes"), ("DomainName", bpaDomainName)]
          . addQueryList (itemQuery $ queryList (attributeQuery setAttributeQuery) "Attribute") "Item" bpaItems
          $ sdbiBaseQuery i

instance SdbFromResponse BatchPutAttributesResponse where
    sdbFromResponse = BatchPutAttributesResponse <$ testElementNameUI "BatchPutAttributesResponse"

instance Transaction BatchPutAttributes SdbInfo (SdbResponse BatchPutAttributesResponse) Error

data Item a
    = Item { itemName :: String, itemData :: a }
    deriving (Show)
             
itemQuery :: (a -> [(String, String)]) -> Item a -> [(String, String)]
itemQuery f (Item name x) = ("ItemName", name) : f x
             
data Select
    = Select {
        sSelectExpression :: String
      , sConsistentRead :: Bool
      , sNextToken :: String
      }
    deriving (Show)

data SelectResponse
    = SelectResponse {
        srItems :: [Item [Attribute String]]
      , srNextToken :: Maybe String
      }
    deriving (Show)

select :: String -> Select
select expr = Select { sSelectExpression = expr, sConsistentRead = False, sNextToken = "" }

instance AsQuery Select SdbInfo where
    asQuery i Select{..}
        = addQuery [("Action", "Select"), ("SelectExpression", sSelectExpression)]
          . addQueryIf sConsistentRead [("ConsistentRead", awsTrue)]
          . addQueryUnless (null sNextToken) [("NextToken", sNextToken)]
          $ sdbiBaseQuery i

instance SdbFromResponse SelectResponse where
    sdbFromResponse = do
      testElementNameUI "SelectResponse"
      items <- inList readItem <<< findElementsNameUI "Item"
      nextToken <- tryMaybe $ strContent <<< findElementNameUI "NextToken"
      return $ SelectResponse items nextToken
          where readItem = do
                        name <- strContent <<< findElementNameUI "Name"
                        attributes <- inList readAttribute <<< findElementsNameUI "Attribute"
                        return $ Item name attributes
                readAttribute = do
                             name <- strContent <<< findElementNameUI "Name"
                             value <- strContent <<< findElementNameUI "Value"
                             return $ ForAttribute name value

instance Transaction Select SdbInfo (SdbResponse SelectResponse) Error
