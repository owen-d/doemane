module PTree where

import           Data.List       (foldl')
import           Data.Map        (Map)
import qualified Data.Map.Strict as Map

data PTree a =
  Node
    { children :: Map a (PTree a)
    }
  deriving (Show, Eq)

empty = Node Map.empty

fromList :: (Ord a, Eq a) => [[a]] -> PTree a
fromList [] = empty
fromList xs =
  foldl' insert empty xs

insert :: (Ord a, Eq a) => PTree a -> [a] -> PTree a
insert tree []  = tree
insert tree tokens
  | (children tree) == Map.empty = Node $ newChild tokens
  | otherwise = Node $ update tokens $ children tree
  where
    newChild []     = Map.empty
    newChild (x:xs) = Map.singleton x (insert empty xs)
    update [] children = children
    update (x:xs) childMap =
      Map.alter updater x childMap where
      updater found = case found of
        Nothing -> Just $ insert empty xs
        Just a  -> Just $ insert a xs
