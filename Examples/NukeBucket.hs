{-# LANGUAGE OverloadedStrings #-}

import qualified Aws
import qualified Aws.S3 as S3
import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
import           Data.Text (pack)
import           Control.Monad ((<=<))
import           Control.Monad.IO.Class (liftIO)
import           Network.HTTP.Client (withManager, responseBody, defaultManagerSettings)
import           System.Environment (getArgs)

main :: IO ()
main = do
  [bucket] <- fmap (map pack) getArgs

  {- Set up AWS credentials and the default configuration. -}
  cfg <- Aws.baseConfiguration
  let s3cfg = Aws.defServiceConfig :: S3.S3Configuration Aws.NormalQuery

  {- Set up a ResourceT region with an available HTTP manager. -}
  withManager defaultManagerSettings $ \mgr -> do
    let src = Aws.awsIteratedSource cfg s3cfg mgr (S3.getBucket bucket)
    let deleteObjects [] = return ()
        deleteObjects os =
          do
            let keys = map S3.objectKey os
            putStrLn ("Deleting objects: " ++ show keys)
            _ <- Aws.pureAws cfg s3cfg mgr (S3.deleteObjects bucket (map S3.objectKey os))
            return ()
    src C.$$ CL.mapM_ (deleteObjects . S3.gbrContents <=< Aws.readResponseIO)
    putStrLn ("Deleting bucket: " ++ show bucket)
    _ <- Aws.pureAws cfg s3cfg mgr (S3.DeleteBucket bucket)
    return ()
