module PTree where

import           Data.Map        (Map)
import qualified Data.Map.Strict as Map

data PTree a = Empty | Node (Map a (PTree a))
  deriving (Show)

fromList :: Ord a => [[a]] -> PTree a
fromList [] = Empty
fromList xs =
  foldl insert Empty xs

insert :: Ord a => PTree a -> [a] -> PTree a
insert tree []  = tree
insert tree tokens =
  case tree of
    Empty         -> Node (newChild tokens)
    Node children -> Node (update tokens children)
  where
    newChild []     = Map.empty
    newChild (x:xs) = Map.singleton x (insert Empty xs)
    update [] children = children
    update (x:xs) children =
      Map.alter updater x children where
      updater found = case found of
        Nothing -> Just $ insert Empty xs
        Just a  -> Just $ insert a xs
