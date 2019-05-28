{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module LibMain where


import           CLI                        (Config (dictionary), runConfig)
import           Data.Aeson                 (FromJSON, ToJSON, encode)
import qualified Data.ByteString.Lazy.Char8 as CL
import           Data.List                  ((\\))
import           Data.Map                   (Map)
import qualified Data.Map.Strict            as Map
import           Data.Maybe                 (catMaybes, fromMaybe)
import           Data.Tuple                 (swap)
import           GHC.Generics               (Generic)
import           PTree                      (PTree)
import qualified PTree
import           Vocab                      (parseInput)

main :: IO ()
main = runConfig run

wordCache :: (Ord k, Eq k, Eq v) => [(v, k)] -> Map k v
wordCache pairs =
  Map.fromList $ map swap pairs

-- builds a wordCache and PTree from a parsed list of (sounds,word) pairs
buildDatabases :: (Ord k, Eq k, Ord v, Eq v) => [([k], v)] -> (Map v [k], PTree k v)
buildDatabases pairs =
  (wordCache pairs, PTree.fromList pairs)

buildPairs :: (Ord a, Eq a) => [[a]] -> [([a], a)]
buildPairs lines =
  catMaybes $ map mapper lines
  where
    mapper []            = Nothing
    mapper (word:sounds) = Just (sounds,word)

every :: Int -> [a] -> [a]
every _ []     = []
every n (x:xs) = x : (every n $ drop n xs)


data Result =
  Result
    { sounds  :: [String]
    , matches :: [[String]]
    , sources :: [String]
    }
  deriving (Show, Generic)

instance FromJSON Result
instance ToJSON Result

run :: Config -> IO ()
run conf = do
  parsed <- (parseInput . dictionary) conf
  case parsed of
    Left e        -> print e
    Right parsed' -> fn parsed'
  where
    fn lines = do
      let pairs = buildPairs lines
      let (_, tree) = buildDatabases pairs
      let smallWords = every 5 $ filter (\(w, _) -> length w < 7) pairs
      let twos = [joinPairs x y | x <- smallWords, y <- smallWords]
      let res =
            map (mkResult tree) twos
      let res' = filter (\x -> (matches x) /= []) res
      print $ take 10 $ res'
      mapM_ (CL.putStrLn . encode) res'
      -- CL.putStrLn $ encode $ take 10 res'
      where
        joinPairs (s1, w1) (s2, w2) =
          ([w1, w2], s1 ++ s2)
        mkResult tree (w, s) = Result {
          sounds = s
          , matches = (fromMaybe [] (PTree.find tree s)) \\ [w]
          , sources = w
          }

