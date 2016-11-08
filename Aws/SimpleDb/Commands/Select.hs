module Aws.SimpleDb.Commands.Select
where

import           Aws.Core
import           Aws.SimpleDb.Core
import           Control.Applicative
import           Control.Monad
import           Data.Maybe
import           Prelude
import           Text.XML.Cursor            (($//), (&|))
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as T
import qualified Text.XML.Cursor            as Cu

data Select
    = Select {
        sSelectExpression :: T.Text
      , sConsistentRead :: Bool
      , sNextToken :: Maybe T.Text
      }
    deriving (Show)

data SelectResponse
    = SelectResponse {
        srItems :: [Item [Attribute T.Text]]
      , srNextToken :: Maybe T.Text
      }
    deriving (Show)

select :: T.Text -> Select
select expr = Select { sSelectExpression = expr, sConsistentRead = False, sNextToken = Nothing }

-- | ServiceConfiguration: 'SdbConfiguration'
instance SignQuery Select where
    type ServiceConfiguration Select = SdbConfiguration
    signQuery Select{..}
        = sdbSignQuery . catMaybes $
            [ Just ("Action", "Select")
            , Just ("SelectExpression", T.encodeUtf8 sSelectExpression)
            , ("ConsistentRead", awsTrue) <$ guard sConsistentRead
            , (("NextToken",) . T.encodeUtf8) <$> sNextToken
            ]

instance ResponseConsumer r SelectResponse where
    type ResponseMetadata SelectResponse = SdbMetadata
    responseConsumer _ _ = sdbResponseConsumer parse
        where parse cursor = do
                sdbCheckResponseType () "SelectResponse" cursor
                items <- sequence $ cursor $// Cu.laxElement "Item" &| readItem
                let nextToken = listToMaybe $ cursor $// elContent "NextToken"
                return $ SelectResponse items nextToken

instance Transaction Select SelectResponse

instance AsMemoryResponse SelectResponse where
    type MemoryResponse SelectResponse = SelectResponse
    loadToMemory = return

instance ListResponse SelectResponse (Item [Attribute T.Text]) where
    listResponse = srItems

instance IteratedTransaction Select SelectResponse where
  nextIteratedRequest req SelectResponse{srNextToken=nt} = req{sNextToken=nt} <$ nt
--  combineIteratedResponse (SelectResponse s1 _) (SelectResponse s2 nt2) = SelectResponse (s1 ++ s2) nt2
