{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Aws.S3.Query
where

import           Aws.Credentials
import           Aws.Http
import           Aws.Query
import           Aws.S3.Info
import           Aws.Signature
import           Aws.Util
import           Control.Applicative
import           Data.List
import           Data.Maybe
import           Data.Monoid
import           Data.Time
import qualified Blaze.ByteString.Builder as Blaze
import qualified Data.Ascii               as A
import qualified Data.ByteString          as B
import qualified Data.ByteString.Lazy     as L
import qualified Network.HTTP.Types       as HTTP

data S3Query
    = S3Query {
        s3QBucket :: Maybe A.Ascii
      , s3QSubresources :: HTTP.Query
      , s3QQuery :: HTTP.Query
      }
    deriving (Show)

s3SignQuery :: S3Query -> S3Info -> SignatureData -> SignedQuery
s3SignQuery S3Query{..} S3Info{..} SignatureData{..}
    = SignedQuery {
        sqMethod = method
      , sqProtocol = s3Protocol
      , sqHost = endpointHost s3Endpoint
      , sqPort = s3Port
      , sqPath = path
      , sqQuery = sortedSubresources ++ s3QQuery ++ authQuery
      , sqDate = Just signatureTime
      , sqAuthorization = authorization
      , sqContentType = contentType
      , sqContentMd5 = contentMd5
      , sqBody = L.empty
      , sqStringToSign = stringToSign
      }
    where
      method = Get
      contentMd5 = Nothing
      contentType = Nothing
      path = mconcat . catMaybes $ [Just "/", s3QBucket]
      sortedSubresources = sort s3QSubresources
      canonicalizedResource = A.fromAsciiBuilder . mconcat . catMaybes $
                              [ Just $ A.toAsciiBuilder "/"
                              , A.toAsciiBuilder <$> s3QBucket
                              , Just $ HTTP.renderQueryBuilder True sortedSubresources
                              ]
      ti = case (s3UseUri, signatureTimeInfo) of
             (False, ti') -> ti'
             (True, AbsoluteTimestamp time) -> AbsoluteExpires $ s3DefaultExpiry `addUTCTime` time
             (True, AbsoluteExpires time) -> AbsoluteExpires time
      sig = signature signatureCredentials HmacSHA1 stringToSign
      stringToSign = B.intercalate "\n" $ concat [[A.toByteString $ httpMethod method]
                                                 , [fromMaybe "" contentMd5]
                                                 , [fromMaybe "" contentType]
                                                 , [A.toByteString $ case ti of
                                                                       AbsoluteTimestamp time -> fmtRfc822Time time
                                                                       AbsoluteExpires time -> fmtTimeEpochSeconds time]
                                                 , [] -- canonicalized AMZ headers
                                                 , [A.toByteString canonicalizedResource]]
      (authorization, authQuery) = case ti of
                                 AbsoluteTimestamp _ -> (Just $ A.unsafeFromByteString $ 
                                                              B.concat ["AWS ", accessKeyID signatureCredentials, ":", sig], [])
                                 AbsoluteExpires time -> (Nothing, HTTP.simpleQueryToQuery $ makeAuthQuery time)
      makeAuthQuery time
          = [("Expires", A.toByteString $ fmtTimeEpochSeconds time)
            , ("AWSAccessKeyId", accessKeyID signatureCredentials)
            , ("SignatureMethod", "HmacSHA256")
            , ("Signature", sig)]
