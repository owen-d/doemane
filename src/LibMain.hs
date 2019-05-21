module LibMain where

import           CLI             (Config (dictionary), runConfig)
import           Data.Map        (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe      (catMaybes)
import           Data.Tuple      (swap)
import           PTree           (PTree, fromList)
import           Vocab           (parseInput)

main :: IO ()
main = runConfig run

wordCache :: (Ord k, Eq k, Eq v) => [(v, k)] -> Map k v
wordCache pairs =
  Map.fromList $ map swap pairs

-- builds a wordCache and PTree from a parsed list of (sounds,word) pairs
buildDatabases :: (Ord k, Eq k, Ord v, Eq v) => [([k], v)] -> (Map v [k], PTree k v)
buildDatabases pairs =
  (wordCache pairs, fromList pairs)

buildPairs :: (Ord a, Eq a) => [[a]] -> [([a], a)]
buildPairs lines =
  catMaybes $ map mapper lines
  where
    mapper []            = Nothing
    mapper (word:sounds) = Just (sounds,word)

run :: Config -> IO ()
run conf = do
  parsed <- parseInput . dictionary $ conf
  case parsed of
    Left e        -> print e
    Right parsed' -> fn parsed'
  where
    fn lines = do
      let (cache, _) = buildDatabases $ buildPairs $ take 5 lines
      print cache
