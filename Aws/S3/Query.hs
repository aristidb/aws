{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Aws.S3.Query
where

import           Aws.Credentials
import           Aws.Http
import           Aws.Query
import           Aws.S3.Info
import           Aws.Signature
import           Aws.Util
import           Data.Function
import           Data.List
import           Data.Maybe
import           Data.Monoid
import           Data.Time
import qualified Blaze.ByteString.Builder       as Blaze
import qualified Blaze.ByteString.Builder.Char8 as Blaze8
import qualified Data.ByteString                as B
import qualified Data.ByteString.Char8          as B8
import qualified Data.CaseInsensitive           as CI
import qualified Network.HTTP.Enumerator        as HTTPE
import qualified Network.HTTP.Types             as HTTP

data S3Query
    = S3Query {
        s3QMethod :: Method
      , s3QBucket :: Maybe B.ByteString
      , s3QObject :: Maybe B.ByteString
      , s3QSubresources :: HTTP.Query
      , s3QQuery :: HTTP.Query
      , s3QAmzHeaders :: HTTP.RequestHeaders
      , s3QRequestBody :: Maybe (HTTPE.RequestBody IO)
      }

instance Show S3Query where
    show S3Query{..} = "S3Query [" ++
                       " method: " ++ show s3QMethod ++
                       " ; bucket: " ++ show s3QBucket ++ 
                       " ; subresources: " ++ show s3QSubresources ++ 
                       " ; query: " ++ show s3QQuery ++
                       " ; request body: " ++ (case s3QRequestBody of Nothing -> "no"; _ -> "yes") ++ 
                       "]"

s3SignQuery :: S3Query -> S3Info -> SignatureData -> SignedQuery
s3SignQuery S3Query{..} S3Info{..} SignatureData{..}
    = SignedQuery {
        sqMethod = s3QMethod
      , sqProtocol = s3Protocol
      , sqHost = B.intercalate "." $ catMaybes host
      , sqPort = s3Port
      , sqPath = mconcat $ catMaybes path
      , sqQuery = sortedSubresources ++ s3QQuery ++ authQuery
      , sqDate = Just signatureTime
      , sqAuthorization = authorization
      , sqContentType = contentType
      , sqContentMd5 = contentMd5
      , sqAmzHeaders = amzHeaders
      , sqBody = s3QRequestBody
      , sqStringToSign = stringToSign
      }
    where
      contentMd5 = Nothing
      contentType = Nothing
      amzHeaders = merge $ sortBy (compare `on` fst) s3QAmzHeaders
          where merge (x1@(k1,v1):x2@(k2,v2):xs) = if k1 == k2
                                                   then (k1, B8.intercalate "," [v1, v2]):merge xs
                                                   else x1:x2:merge xs
                merge xs = xs
      
      (host, path) = case s3RequestStyle of 
                       PathStyle   -> ([Just s3Endpoint], [Just "/", fmap (`B8.snoc` '/') s3QBucket, s3QObject])
                       BucketStyle -> ([s3QBucket, Just s3Endpoint], [Just "/", s3QObject])
                       VHostStyle  -> ([Just $ fromMaybe s3Endpoint s3QBucket], [Just "/", s3QObject])
      sortedSubresources = sort s3QSubresources
      canonicalizedResource = Blaze8.fromChar '/' `mappend`
                              maybe mempty (\s -> Blaze.copyByteString s `mappend` Blaze8.fromChar '/') s3QBucket `mappend`
                              maybe mempty Blaze.copyByteString s3QObject `mappend`
                              HTTP.renderQueryBuilder True sortedSubresources
      ti = case (s3UseUri, signatureTimeInfo) of
             (False, ti') -> ti'
             (True, AbsoluteTimestamp time) -> AbsoluteExpires $ s3DefaultExpiry `addUTCTime` time
             (True, AbsoluteExpires time) -> AbsoluteExpires time
      sig = signature signatureCredentials HmacSHA1 stringToSign
      stringToSign = Blaze.toByteString . mconcat . intersperse (Blaze8.fromChar '\n') . concat  $
                       [[Blaze.copyByteString $ httpMethod s3QMethod]
                       , [maybe mempty Blaze.copyByteString contentMd5]
                       , [maybe mempty Blaze.copyByteString contentType]
                       , [Blaze.copyByteString $ case ti of
                                                   AbsoluteTimestamp time -> fmtRfc822Time time
                                                   AbsoluteExpires time -> fmtTimeEpochSeconds time]
                       , map amzHeader amzHeaders
                       , [canonicalizedResource]
                       ]
          where amzHeader (k, v) = Blaze.copyByteString (CI.foldedCase k) `mappend` Blaze8.fromChar ':' `mappend` Blaze.copyByteString v
      (authorization, authQuery) = case ti of
                                 AbsoluteTimestamp _ -> (Just $ B.concat ["AWS ", accessKeyID signatureCredentials, ":", sig], [])
                                 AbsoluteExpires time -> (Nothing, HTTP.simpleQueryToQuery $ makeAuthQuery time)
      makeAuthQuery time
          = [("Expires", fmtTimeEpochSeconds time)
            , ("AWSAccessKeyId", accessKeyID signatureCredentials)
            , ("SignatureMethod", "HmacSHA256")
            , ("Signature", sig)]
