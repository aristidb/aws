{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Aws
import           Aws.Core
import           Aws.DynamoDb.Commands
import           Aws.DynamoDb.Core



main :: IO ()
main = do
  {- Load configuration -}
  cfg <- Aws.baseConfiguration

  putStrLn "Making request..."

  {- Make request -}

  let req = GetItem "devel-1" (HPK (DString "oz")) Nothing False
  (resp :: GetItemResponse) <- Aws.simpleAws cfg debugServiceConfig req

  {- Analyze response -}
  putStrLn "Response: "
  print resp



