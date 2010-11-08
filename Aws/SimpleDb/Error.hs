{-# LANGUAGE DeriveDataTypeable #-}
module Aws.SimpleDb.Error
where

import           Aws.Metadata
import           Control.Monad.Error.Class
import           Data.Maybe
import           Data.Typeable
import           Text.XML.Monad
import qualified Control.Exception         as C
import qualified Data.Map                  as M

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

data ErrorCode
    = AccessFailure
    | AttributeDoesNotExist
    | AttributesExceeded
    | AuthFailure
    | AuthMissingFailure
    | ConditionalCheckFailed
    | ExistsAndExpectedValue
    | FeatureDeprecated
    | IncompleteExpectedExpression
    | InternalError
    | InvalidAction
    | InvalidHttpAuthHeader
    | InvalidHttpRequest
    | InvalidLiteral
    | InvalidNextToken
    | InvalidNumberPredicates
    | InvalidNumberValueTests
    | InvalidParameterCombination
    | InvalidParameterValue
    | InvalidQueryExpression
    | InvalidResponseGroups
    | InvalidSoapRequest
    | InvalidService
    | InvalidSortExpression
    | InvalidUri
    | InvalidWsAddressingProperty
    | InvalidWsdlVersion
    | MalformedSoapSignature
    | MissingAction
    | MissingParameter
    | MissingWsAddressingProperty
    | MultiValuedAttribute
    | MultipleExistsConditions
    | MultipleExpectedNames
    | MultipleExpectedValues
    | NoSuchDomain
    | NoSuchVersion
    | NotYetImplemented
    | NumberDomainAttributes
    | NumberDomainBytesExceeded
    | NumberDomainsExceeded
    | NumberItemAttributes
    | NumberSubmitted
    | NumberSubmittedAttributesExceeded
    | NumberSubmittedItemsExceeded
    | RequestExpired
    | RequestTimeout
    | ServiceUnavailable
    | TooManyRequestedAttributes
    | UriTooLong
    | UnsupportedHttpVerb
    | UnsupportedNextToken
    | UnrecognizedErrorCode String
    deriving (Eq, Ord)

errorCodeStrings :: [(ErrorCode, String)]
errorCodeStrings = [(AccessFailure, "AccessFailure")
                   , (AttributeDoesNotExist, "AttributeDoesNotExist")
                   , (AttributesExceeded, "AttributesExceeded")
                   , (AuthFailure, "AuthFailure")
                   , (AuthMissingFailure, "AuthMissingFailure")
                   , (ConditionalCheckFailed, "ConditionalCheckFailed")
                   , (ExistsAndExpectedValue, "ExistsAndExpectedValue")
                   , (FeatureDeprecated, "FeatureDeprecated")
                   , (IncompleteExpectedExpression, "IncompleteExpectedExpression")
                   , (InternalError, "InternalError")
                   , (InvalidAction, "InvalidAction")
                   , (InvalidHttpAuthHeader, "InvalidHTTPAuthHeader")
                   , (InvalidHttpRequest, "InvalidHttpRequest")
                   , (InvalidLiteral, "InvalidLiteral")
                   , (InvalidNextToken, "InvalidNextToken")
                   , (InvalidNumberPredicates, "InvalidNumberPredicates")
                   , (InvalidNumberValueTests, "InvalidNumberValueTests")
                   , (InvalidParameterCombination, "InvalidParameterCombination")
                   , (InvalidParameterValue, "InvalidParameterValue")
                   , (InvalidQueryExpression, "InvalidQueryExpression")
                   , (InvalidResponseGroups, "InvalidResponseGroups")
                   , (InvalidSoapRequest, "InvalidSOAPRequest")
                   , (InvalidService, "InvalidService")
                   , (InvalidSortExpression, "InvalidSortExpression")
                   , (InvalidUri, "InvalidURI")
                   , (InvalidWsAddressingProperty, "InvalidWSAddressingProperty")
                   , (InvalidWsdlVersion, "InvalidWSDLVersion")
                   , (MalformedSoapSignature, "MalformedSOAPSignature")
                   , (MissingAction, "MissingAction")
                   , (MissingParameter, "MissingParameter")
                   , (MissingWsAddressingProperty, "MissingWSAddressingProperty")
                   , (MultiValuedAttribute, "MultiValuedAttribute")
                   , (MultipleExistsConditions, "MultipleExistsConditions")
                   , (MultipleExpectedNames, "MultipleExpectedNames")
                   , (MultipleExpectedValues, "MultipleExpectedValues")
                   , (NoSuchDomain, "NoSuchDomain")
                   , (NoSuchVersion, "NoSuchVersion")
                   , (NotYetImplemented, "NotYetImplemented")
                   , (NumberDomainAttributes, "NumberDomainAttributes")
                   , (NumberDomainBytesExceeded, "NumberDomainBytesExceeded")
                   , (NumberDomainsExceeded, "NumberDomainsExceeded")
                   , (NumberItemAttributes, "NumberItemAttributes")
                   , (NumberSubmitted, "NumberSubmitted")
                   , (NumberSubmittedAttributesExceeded, "NumberSubmittedAttributesExceeded")
                   , (NumberSubmittedItemsExceeded, "NumberSubmittedItemsExceeded")
                   , (RequestExpired, "RequestExpired")
                   , (RequestTimeout, "RequestTimeout")
                   , (ServiceUnavailable, "ServiceUnavailable")
                   , (TooManyRequestedAttributes, "TooManyRequestedAttributes")
                   , (UriTooLong, "URITooLong")
                   , (UnsupportedHttpVerb, "UnsupportedHttpVerb")
                   , (UnsupportedNextToken, "UnsupportedNextToken")
                   ]

errorCodeToNameMap :: M.Map ErrorCode String
errorCodeToNameMap = M.fromList errorCodeStrings

nameToErrorCodeMap :: M.Map String ErrorCode
nameToErrorCodeMap = M.fromList . map (\(a, b) -> (b, a)) $ errorCodeStrings

errorCodeToName :: ErrorCode -> String
errorCodeToName (UnrecognizedErrorCode x) = x
errorCodeToName x = fromJust $ M.lookup x errorCodeToNameMap

nameToErrorCode :: String -> ErrorCode
nameToErrorCode x = fromMaybe (UnrecognizedErrorCode x) (M.lookup x nameToErrorCodeMap)

instance Show ErrorCode where
    show = errorCodeToName

instance Read ErrorCode where
    readsPrec _ = readParen False (\x -> [(nameToErrorCode x, "")])
