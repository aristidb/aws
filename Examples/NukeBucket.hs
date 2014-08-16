{-# LANGUAGE OverloadedStrings #-}

import qualified Aws
import qualified Aws.S3 as S3
import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
import           Data.Text (pack)
import           Control.Monad ((<=<))
import           Control.Monad.Trans.Resource (runResourceT)
import           Control.Monad.IO.Class (liftIO)
import           Network.HTTP.Conduit (withManager, responseBody)
import           System.Environment (getArgs)

main :: IO ()
main = Aws.withDefaultEnvironment $ \env -> runResourceT $ do
  [bucket] <- liftIO $ fmap (map pack) getArgs

  let src = Aws.awsIteratedSource env (S3.getBucket bucket)
  let deleteObjects [] = return ()
      deleteObjects os =
        do
          let keys = map S3.objectKey os
          liftIO $ putStrLn ("Deleting objects: " ++ show keys)
          _ <- Aws.pureAws env (S3.deleteObjects bucket (map S3.objectKey os))
          return ()
  src C.$$ CL.mapM_ (deleteObjects . S3.gbrContents <=< Aws.readResponseIO)
  liftIO $ putStrLn ("Deleting bucket: " ++ show bucket)
  _ <- Aws.pureAws env (S3.DeleteBucket bucket)
  return ()
