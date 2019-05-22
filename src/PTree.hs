module PTree where

import           Data.List       (foldl')
import           Data.Map        (Map)
import qualified Data.Map.Strict as Map

data PTree k v =
  Node
    { children     :: Map k (PTree k v)
    , terminations :: [v]
    }
  deriving (Show, Eq)

empty = Node {children = Map.empty, terminations = []}

fromList :: (Ord k, Eq k, Eq v) => [([k], v)] -> PTree k v
fromList [] = empty
fromList xs =
  foldl' (\tree (keys, val) -> insert tree keys val) empty xs

insert :: (Ord k, Eq k, Eq v) => PTree k v -> [k] -> v -> PTree k v
insert tree [] val = tree {terminations = (terminations tree) ++ [val]}
insert tree tokens val
  | (children tree) == Map.empty = tree {children = newChild tokens val}
  | otherwise = tree {children = update tokens $ children tree}
  where
    newChild [] _       = Map.empty
    newChild (x:xs) val = Map.singleton x (insert empty xs val)
    -- -- this shouldnt fire as the [] match on tokens should be exhausted by the first insert definition
    update [] children = children
    update (x:xs) childMap = Map.alter updater x childMap
      where
        updater found =
          case found of
            Nothing -> Just $ insert empty xs val
            Just a  -> Just $ insert a xs val

find :: (Ord k, Eq k, Eq v) => PTree k v -> [k] -> Maybe [[v]]
find tree ks = findWithRoot tree tree ks

-- findWithRoot keeps a reference to the root of the tree so that it can re-descend upon
-- reaching early terminations (i.e. 'domain' -> [['domain'], ['doe', 'mane']'])
findWithRoot :: (Ord k, Eq k, Eq v) => PTree k v -> PTree k v -> [k] -> Maybe [[v]]
-- take terminations=[word1, word2] -> [[word1], [word2]] for consistency in return fmt
findWithRoot _ (Node {terminations = terms}) [] =
  if (terms == [])
    then Nothing
    else Just $ map (\x -> [x]) terms
findWithRoot root tree (k:ks) = prefixMatches tree <> childMatches
  where
    childMatches =
      Map.lookup k (children tree) >>= \found -> findWithRoot root found ks
    prefixMatches Node {terminations = terms}
      | terms == [] = Nothing
      | otherwise =
        fmap (\matches -> [x : ys | x <- terms, ys <- matches]) subMatches
      where
        subMatches = findWithRoot root root (k : ks)
