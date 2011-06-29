{-# LANGUAGE OverloadedStrings,RecordWildCards #-}

module Aws.SQS.Query where

import           Aws.Credentials
import           Aws.Http
import           Aws.Query
import           Aws.Signature
import           Aws.SQS.Info
import           Aws.Signature
import           Aws.Util
import           Data.List
import           Data.Maybe
import           Data.Monoid
import           Data.Ord
import           Data.Time
import           Data.Time.Format
import           System.Locale
import qualified Blaze.ByteString.Builder       as Blaze
import qualified Blaze.ByteString.Builder.Char8 as Blaze8
import qualified Data.ByteString                as B
import qualified Data.ByteString.Char8          as BC
import qualified Data.ByteString.Lazy           as L
import qualified Network.HTTP.Types             as HTTP
import Debug.Trace

data SqsQuery = SqsQuery{
  sqsQueueName :: Maybe String,
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
      , sqBody = L.empty
      , sqStringToSign = stringToSign
      , sqContentType = Nothing
      , sqContentMd5 = Nothing
      }
    where
      method = PostQuery
      path = case sqsQueueName of
                Just x -> B.concat ["/", BC.pack x, "/"]
                Nothing -> "/"
      expandedQuery = sortBy (comparing fst) 
                       ( sqsQuery ++ [ ("AWSAccessKeyId", Just(accessKeyID signatureCredentials)), 
                       ("Expires", Just(BC.pack expiresString)), 
                       ("SignatureMethod", Just("HmacSHA256")), ("SignatureVersion",Just("2")), ("Version",Just("2009-02-01"))

                       ])
      
      expires = AbsoluteExpires $ sqsDefaultExpiry `addUTCTime` signatureTime

      ti = case (sqsUseUri, signatureTimeInfo) of
             (False, ti') -> ti'
             (True, AbsoluteTimestamp time) -> AbsoluteExpires $ sqsDefaultExpiry `addUTCTime` time
             (True, AbsoluteExpires time) -> AbsoluteExpires $ sqsDefaultExpiry `addUTCTime` time

      expiresString = formatTime defaultTimeLocale "%FT%TZ" (fromAbsoluteTimeInfo expires)

      sig = signature signatureCredentials HmacSHA256 stringToSign
      stringToSign = Blaze.toByteString . mconcat . intersperse (Blaze8.fromChar '\n') . concat  $
                       [[Blaze.copyByteString $ httpMethod method]
                       , [Blaze.copyByteString $ endpointHost sqsEndpoint]
                       , [Blaze.copyByteString path]
                       , [Blaze.copyByteString $ HTTP.renderQuery False expandedQuery ]]

      signedQuery = expandedQuery ++ (HTTP.simpleQueryToQuery $ makeAuthQuery)

      (authorization, authQuery) = case ti of
                                 AbsoluteTimestamp _ -> (Just $ B.concat ["AWS ", accessKeyID signatureCredentials, ":", sig], [])
                                 AbsoluteExpires time -> (Nothing, HTTP.simpleQueryToQuery $ makeAuthQuery)
      makeAuthQuery = [("Signature", sig)]
