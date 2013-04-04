{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

-------------------------------------------------------------------------------
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

  let x = item [ attrAs text "name" "josh"
               , attrAs text "class" "not-so-awesome"]

  let req1 = putItem "devel-1" x


  (resp1 :: PutItemResponse) <- Aws.simpleAws cfg debugServiceConfig req1
  putStrLn "Response: "
  print resp1

  putStrLn "Getting the item back..."

  {- Make request -}

  let req2 = GetItem "devel-1" (hpk ("josh" :: T.Text)) Nothing False
  (resp2 :: GetItemResponse) <- Aws.simpleAws cfg debugServiceConfig req2

  {- Analyze response -}
  putStrLn "Response: "
  print resp2



