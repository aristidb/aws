module Aws.Xml
where
  
import           Data.Enumerator              ((=$))
import           Text.XML.Enumerator.Cursor
import           Control.Exception
import qualified Data.ByteString              as B
import qualified Data.Enumerator              as En
import qualified Data.Text                    as T
import qualified Network.HTTP.Types           as HTTP
import qualified Text.XML.Enumerator.Cursor   as Cu
import qualified Text.XML.Enumerator.Parse    as XML
import qualified Text.XML.Enumerator.Resolved as XML

elCont :: T.Text -> Cursor -> [String]
elCont name = laxElement name &/ content &| T.unpack

force :: e -> [a] -> Either e a
force e []    = Left e
force _ (x:_) = Right x

forceM :: e -> [Either e a] -> Either e a
forceM e []    = Left e
forceM _ (x:_) = x

readInt :: Num a => e -> String -> Either e a
readInt e s = case reads s of
                [(n,"")] -> Right $ fromInteger n
                _        -> Left e

xmlCursorIteratee :: 
    (Exception e)
    => (Cu.Cursor -> Either e a) 
    -> HTTP.Status 
    -> HTTP.ResponseHeaders 
    -> En.Iteratee B.ByteString IO a
xmlCursorIteratee parse _status _headers
    = do doc <- XML.parseBytes XML.decodeEntities =$ XML.fromEvents
         let cursor = Cu.fromDocument doc
         case parse cursor of                                  
           Left err -> En.throwError err
           Right v -> return v
