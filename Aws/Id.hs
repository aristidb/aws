{-# LANGUAGE TemplateHaskell #-}
module Aws.Id
where
  
import           Aws.Util
import           Control.Applicative
import           Data.Char
import           Data.List
import           Data.Maybe
import           Data.Ord
import           Language.Haskell.TH
import qualified Data.Map            as M

makeId :: String -> Q Exp -> [(String, String)] -> Q [Dec]
makeId name handler xs = do
  let (ctors, ids) = unzip xs
  
  let [idName, idFromId, idToId] = map mkName [name, "from" ++ name, "to" ++ name]
  [idListA, idMapA, idListB, idMapB] <- mapM newName ["idListA", "idMapA", "idListB", "idMapB"]

  data' <- dataD (return []) (idName) [] (map (flip normalC [] . mkName) ctors) [mkName "Eq", mkName "Ord", mkName "Show"]

  let valDecl var body = valD (varP var) (normalB body) []

  listA <- valDecl idListA (listE $ map (\(c,i) -> tupE [conE $ mkName c, litE $ stringL i]) xs)
  mapA <- valDecl idMapA [| M.fromList $(varE idListA) |]
  listB <- valDecl idListB [| map (\(c,i) -> (i,c)) $(varE idListA) |]
  mapB <- valDecl idMapB [| M.fromList $(varE idListB) |]

  fromId <- valDecl idFromId [| fromJust . flip M.lookup $(varE idMapA) |]
  toId <- valDecl idToId [| $handler . flip M.lookup $(varE idMapB) |]

  return [data', listA, mapA, listB, mapB, fromId, toId]

makeIdAuto :: String -> Q Exp -> (String -> String) -> [String] -> Q [Dec]
makeIdAuto name handler f xs = makeId name handler xs'
    where xs' = map (\a -> (f a, a)) xs

capitalise :: String -> String
capitalise "" = ""
capitalise (x:xs) = toUpper x : map toLower xs

adjustReplace :: Eq a => ([a] -> [a]) -> [[a]] -> [a] -> [a]
adjustReplace f = searchReplace . map (\s -> (s, f s))

searchReplace :: Eq a => [([a], [a])] -> [a] -> [a]
searchReplace abs xs = case bestSearch abs xs of
                         Just (pre, b, post) -> pre ++ b ++ searchReplace abs post
                         Nothing -> xs

bestSearch :: (Eq a) => [([a], b)] -> [a] -> Maybe ([a], b, [a])
bestSearch = best .: (sequence . map search)
  where best = listToMaybe . sortBy (comparing $ \(pre, _, _) -> length pre) . catMaybes

search :: Eq a => ([a], b) -> [a] -> Maybe ([a], b, [a])
search (k, k') = listToMaybe . mapMaybe (\(i, t) -> if k `isPrefixOf` t then Just (i, k', drop (length k) t) else Nothing) . (zip <$> inits <*> tails)
