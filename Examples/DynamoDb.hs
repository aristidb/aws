{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import qualified Data.Text             as T
-------------------------------------------------------------------------------
import           Aws
import           Aws.Core
import           Aws.DynamoDb.Commands
import           Aws.DynamoDb.Core
-------------------------------------------------------------------------------



main :: IO ()
main = do
  {- Load configuration -}
  cfg <- Aws.baseConfiguration

  putStrLn "Putting an item..."

  let req1 = putItem {
               piTable = "devel-1"
             , piItem = item [ attr "name" ("josh" :: T.Text)
                             , attr "class" ("not-so-awesome" :: T.Text) ]
             }

  (resp1 :: PutItemResponse) <- Aws.simpleAws cfg debugServiceConfig req1

  putStrLn "Getting the item back..."

  {- Make request -}

  let req2 = GetItem "devel-1" (hpk ("josh" :: T.Text)) Nothing False
  (resp2 :: GetItemResponse) <- Aws.simpleAws cfg debugServiceConfig req2

  {- Analyze response -}
  putStrLn "Response: "
  print resp2



