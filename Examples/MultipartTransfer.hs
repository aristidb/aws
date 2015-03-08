{-# LANGUAGE OverloadedStrings #-}

{- This example demonstrates an ability to stream in constant space content from a remote resource into an S3 object accessible publicly -}


import qualified Aws
import           Aws.Aws              (Configuration (..))
import qualified Aws.S3               as S3
import           Control.Applicative  ((<$>))
import           Data.Conduit         (unwrapResumable)
import qualified Data.Text            as T
import           Network.HTTP.Conduit (http, parseUrl, responseBody,
                                       withManager)
import           System.Environment   (getArgs)

main :: IO ()
main = do
  maybeCreds <- Aws.loadCredentialsFromEnv
  case maybeCreds of
    Nothing -> do
      putStrLn "Please set the environment variables AWS_ACCESS_KEY_ID and AWS_ACCESS_KEY_SECRET"
    Just creds -> do
      args <- getArgs
      cfg <- Aws.dbgConfiguration
      let s3cfg = Aws.defServiceConfig :: S3.S3Configuration Aws.NormalQuery

      case args of
        [sourceUrl,destBucket,destObj] -> do
          request <- parseUrl sourceUrl
          withManager $ \mgr -> do
            resumableSource <- responseBody <$> http request mgr
            (source, _) <- unwrapResumable resumableSource
            let initiator b o = (S3.postInitiateMultipartUpload b o){S3.imuAcl = Just S3.AclPublicRead}
            S3.multipartUploadWithInitiator cfg{credentials = creds} s3cfg initiator mgr (T.pack destBucket) (T.pack destObj) source (10*1024*1024)
        _ -> do
          putStrLn "Usage: MultipartTransfer sourceUrl destinationBucket destinationObjectname"

