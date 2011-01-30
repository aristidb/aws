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

makeId :: String -> Q Exp -> [(String, String)] -> [ConQ] -> Q [Dec]
makeId name handler xs otherCon = do
  let (ctors, ids) = unzip xs
  
  let [idName, idFromId, idToId] = map mkName [name, "from" ++ name, "to" ++ name]
  [idListA, idMapA, idListB, idMapB] <- mapM newName ["idListA", "idMapA", "idListB", "idMapB"]

  data' <- dataD (cxt []) idName [] (map (flip normalC [] . mkName) ctors ++ otherCon) [''Eq, ''Ord, ''Show]

  let valDecl var body = valD (varP var) (normalB body) []

  listAT <- sigD idListA [t| [($(conT idName), String)] |]
  listA <- valDecl idListA (listE $ map (\(c,i) -> tupE [conE $ mkName c, litE $ stringL i]) xs)

  mapAT <- sigD idMapA [t| M.Map $(conT idName) String |]
  mapA <- valDecl idMapA [| M.fromList $(varE idListA) |]

  listBT <- sigD idListB [t| [(String, $(conT idName))] |]
  listB <- valDecl idListB [| map (\(c,i) -> (i,c)) $(varE idListA) |]

  mapBT <- sigD idMapB [t| M.Map String $(conT idName) |]
  mapB <- valDecl idMapB [| M.fromList $(varE idListB) |]

  fromIdT <- sigD idFromId [t| $(conT idName) -> String |]
  fromId <- valDecl idFromId [| fromJust . flip M.lookup $(varE idMapA) |]

  --toIdT <- sigD idToId [t| String -> Maybe $(conT idName) |]
  toId <- valDecl idToId [| \s -> $handler s $ M.lookup s $(varE idMapB) |]

  return [data', listAT, listA, mapAT, mapA, listBT, listB, mapBT, mapB, fromIdT, fromId, {-toIdT,-} toId]

unknownC :: Name -> [ConQ]
unknownC unknownName = [normalC unknownName [strictType notStrict $ conT ''String]]

makeIdUnknown :: String -> [(String, String)] -> String -> Q [Dec]
makeIdUnknown name xs unknownNameS = makeId name [| fromMaybe . $(conE unknownName) |] xs (unknownC unknownName)
    where unknownName = mkName unknownNameS

autoTable :: (String -> String) -> [String] -> [(String, String)]
autoTable f = map (\a -> (f a, a))

autoCapitalise :: [String] -> [String] -> [(String, String)]
autoCapitalise cap xs = autoTable (adjustReplace capitalise cap) xs

capitalise :: String -> String
capitalise "" = ""
capitalise (x:xs) = toUpper x : map toLower xs

adjustReplace :: Eq a => ([a] -> [a]) -> [[a]] -> [a] -> [a]
adjustReplace f = searchReplace . map (\s -> (s, f s))

searchReplace :: Eq a => [([a], [a])] -> [a] -> [a]
searchReplace rs xs = case bestSearch rs xs of
                         Just (pre, b, post) -> pre ++ b ++ searchReplace rs post
                         Nothing -> xs

bestSearch :: (Eq a) => [([a], b)] -> [a] -> Maybe ([a], b, [a])
bestSearch = best .: (sequence . map search)
  where best = listToMaybe . sortBy (comparing $ \(pre, _, _) -> length pre) . catMaybes

search :: Eq a => ([a], b) -> [a] -> Maybe ([a], b, [a])
search (k, k') = listToMaybe . mapMaybe (\(i, t) -> if k `isPrefixOf` t then Just (i, k', drop (length k) t) else Nothing) . (zip <$> inits <*> tails)
