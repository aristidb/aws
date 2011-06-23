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

xmlCursorIteratee :: 
    (Exception e)
    => (HTTP.Status -> HTTP.ResponseHeaders -> Cu.Cursor -> Either e a) 
    -> HTTP.Status 
    -> HTTP.ResponseHeaders 
    -> En.Iteratee B.ByteString IO a
xmlCursorIteratee parse status headers
    = do doc <- XML.parseBytes XML.decodeEntities =$ XML.fromEvents
         let cursor = Cu.fromDocument doc
         case parse status headers cursor of                                  
           Left err -> En.throwError err
           Right v -> return v
