{-# LANGUAGE OverloadedStrings,RecordWildCards #-}

module Aws.Sqs.Query where

import           Aws.Core
import           Aws.Sqs.Info
import           Aws.Sqs.Model
import           Data.List
import           Data.Monoid
import           Data.Ord
import           Data.Time
import           System.Locale
import qualified Blaze.ByteString.Builder       as Blaze
import qualified Blaze.ByteString.Builder.Char8 as Blaze8
import qualified Data.ByteString.Char8          as BC
import qualified Data.Text.Encoding             as TE
import qualified Network.HTTP.Types             as HTTP

data SqsQuery = SqsQuery{
  sqsQueueName :: Maybe QueueName,
  sqsQuery :: HTTP.Query
}

sqsSignQuery :: SqsQuery -> SqsInfo -> SignatureData -> SignedQuery
sqsSignQuery SqsQuery{..} SqsInfo{..} SignatureData{..}
    = SignedQuery {
        sqMethod = method
      , sqProtocol = sqsProtocol
      , sqHost = endpointHost sqsEndpoint
      , sqPort = sqsPort
      , sqPath = path
      , sqQuery = signedQuery
      , sqDate = Just signatureTime
      , sqAuthorization = Nothing 
      , sqBody = Nothing
      , sqStringToSign = stringToSign
      , sqContentType = Nothing
      , sqContentMd5 = Nothing
      , sqAmzHeaders = []
      , sqOtherHeaders = []
      }
    where
      method = PostQuery
      path = case sqsQueueName of
                Just x -> TE.encodeUtf8 $ printQueueName x
                Nothing -> "/"
      expandedQuery = sortBy (comparing fst) 
                       ( sqsQuery ++ [ ("AWSAccessKeyId", Just(accessKeyID signatureCredentials)), 
                       ("Expires", Just(BC.pack expiresString)), 
                       ("SignatureMethod", Just("HmacSHA256")), ("SignatureVersion",Just("2")), ("Version",Just("2009-02-01"))

                       ])
      
      expires = AbsoluteExpires $ sqsDefaultExpiry `addUTCTime` signatureTime

      expiresString = formatTime defaultTimeLocale "%FT%TZ" (fromAbsoluteTimeInfo expires)

      sig = signature signatureCredentials HmacSHA256 stringToSign
      stringToSign = Blaze.toByteString . mconcat . intersperse (Blaze8.fromChar '\n') . concat  $
                       [[Blaze.copyByteString $ httpMethod method]
                       , [Blaze.copyByteString $ endpointHost sqsEndpoint]
                       , [Blaze.copyByteString path]
                       , [Blaze.copyByteString $ HTTP.renderQuery False expandedQuery ]]

      signedQuery = expandedQuery ++ (HTTP.simpleQueryToQuery $ makeAuthQuery)

      makeAuthQuery = [("Signature", sig)]
