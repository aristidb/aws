module Aws.Xml
where
  
import           Text.XML.Enumerator.Cursor
import qualified Data.Text                    as T
import qualified Text.XML.Enumerator.Parse    as XML
import qualified Text.XML.Enumerator.Resolved as XML

elCont :: T.Text -> Cursor -> [String]
elCont name = laxElement name &/ content &| T.unpack

force :: e -> [a] -> Either e a
force e []    = Left e
force _ (x:_) = Right x
