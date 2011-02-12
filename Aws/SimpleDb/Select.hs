{-# LANGUAGE RecordWildCards, MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, TypeFamilies, OverloadedStrings #-}

module Aws.SimpleDb.Select
where

import           Aws.Query
import           Aws.Signature
import           Aws.SimpleDb.Info
import           Aws.SimpleDb.Model
import           Aws.SimpleDb.Query
import           Aws.SimpleDb.Response
import           Aws.Transaction
import           Control.Monad
import           Control.Monad.Compose.Class
import           Text.XML.Monad
import qualified Data.ByteString.UTF8        as BU

data Select
    = Select {
        sSelectExpression :: String
      , sConsistentRead :: Bool
      , sNextToken :: String
      }
    deriving (Show)

data SelectResponse
    = SelectResponse {
        srItems :: [Item [Attribute String]]
      , srNextToken :: Maybe String
      }
    deriving (Show)

select :: String -> Select
select expr = Select { sSelectExpression = expr, sConsistentRead = False, sNextToken = "" }

instance SignQuery Select where
    type Info Select = SdbInfo
    signQuery Select{..}
        = sdbSignQuery $
            [("Action", "Select"), ("SelectExpression", BU.fromString sSelectExpression)] ++
            (guard sConsistentRead >> [("ConsistentRead", awsTrue)]) ++
            (guard (not $ null sNextToken) >> [("NextToken", BU.fromString sNextToken)])

instance SdbFromResponse SelectResponse where
    sdbFromResponse = do
      testElementNameUI "SelectResponse"
      items <- inList readItem <<< findElementsNameUI "Item"
      nextToken <- tryMaybe $ strContent <<< findElementNameUI "NextToken"
      return $ SelectResponse items nextToken

instance Transaction Select (SdbResponse SelectResponse)
