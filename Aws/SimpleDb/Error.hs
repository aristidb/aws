{-# LANGUAGE DeriveDataTypeable, TemplateHaskell #-}
module Aws.SimpleDb.Error
where

import           Aws.Id
import           Aws.Metadata
import           Control.Monad.Error.Class
import           Data.Typeable
import           Text.XML.Monad
import qualified Control.Exception         as C

$(makeIdUnknown "ErrorCode"
                    (autoCapitalise
                     ["HTTP", "SOAP", "URI", "WSDL", "WS"]
                     ["AccessFailure"
                     ,"AttributeDoesNotExist"
                     ,"AttributesExceeded"
                     ,"AuthFailure"
                     ,"AuthMissingFailure"
                     ,"ConditionalCheckFailed"
                     ,"ExistsAndExpectedValue"
                     ,"FeatureDeprecated"
                     ,"IncompleteExpectedExpression"
                     ,"InternalError"
                     ,"InvalidAction"
                     ,"InvalidHTTPAuthHeader"
                     ,"InvalidHttpRequest"
                     ,"InvalidLiteral"
                     ,"InvalidNextToken"
                     ,"InvalidNumberPredicates"
                     ,"InvalidNumberValueTests"
                     ,"InvalidParameterCombination"
                     ,"InvalidParameterValue"
                     ,"InvalidQueryExpression"
                     ,"InvalidResponseGroups"
                     ,"InvalidSOAPRequest"
                     ,"InvalidService"
                     ,"InvalidSortExpression"
                     ,"InvalidURI"
                     ,"InvalidWSAddressingProperty"
                     ,"InvalidWSDLVersion"
                     ,"MalformedSOAPSignature"
                     ,"MissingAction"
                     ,"MissingParameter"
                     ,"MissingWSAddressingProperty"
                     ,"MultiValuedAttribute"
                     ,"MultipleExistsConditions"
                     ,"MultipleExpectedNames"
                     ,"MultipleExpectedValues"
                     ,"NoSuchDomain"
                     ,"NoSuchVersion"
                     ,"NotYetImplemented"
                     ,"NumberDomainAttributes"
                     ,"NumberDomainBytesExceeded"
                     ,"NumberDomainsExceeded"
                     ,"NumberItemAttributes"
                     ,"NumberSubmitted"
                     ,"NumberSubmittedAttributesExceeded"
                     ,"NumberSubmittedItemsExceeded"
                     ,"RequestExpired"
                     ,"RequestTimeout"
                     ,"ServiceUnavailable"
                     ,"TooManyRequestedAttributes"
                     ,"URITooLong"
                     ,"UnsupportedHttpVerb"
                     ,"UnsupportedNextToken"])
                    "UnrecognizedErrorCode")

data SdbError
    = SdbError {
        sdbStatusCode :: Int
      , sdbErrorCode :: ErrorCode
      , sdbErrorMessage :: String
      , sdbErrorMetadata :: Metadata
      }
    | SdbXmlError { 
        fromSdbXmlError :: XmlError
      , sdbXmlErrorMetadata :: Metadata
      }
    deriving (Show, Typeable)

instance FromXmlError SdbError where
    fromXmlError = flip SdbXmlError NoMetadata

instance WithMetadata SdbError where
    getMetadata SdbError { sdbErrorMetadata = err }       = err
    getMetadata SdbXmlError { sdbXmlErrorMetadata = err } = err

    setMetadata m e@SdbError{}    = e { sdbErrorMetadata = m }
    setMetadata m e@SdbXmlError{} = e { sdbXmlErrorMetadata = m }

instance Error SdbError where
    noMsg = fromXmlError noMsg
    strMsg = fromXmlError . strMsg

instance C.Exception SdbError
