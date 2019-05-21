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

-- x = fromList [(['a', 'b', 'c'], "abc"), (['a', 'b', 'c', 'd'], "abcd"), (['a', 'x', 'y'], "axy")]
