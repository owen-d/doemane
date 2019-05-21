module LibMain where

import           CLI        (Config (dictionary), runConfig)
import           Data.Maybe (catMaybes)
import           Vocab      (parseInput)


main :: IO ()
main = runConfig run

run :: Config -> IO ()
run conf = do
  parsed <- parseInput . dictionary $ conf
  print $ fmap fn parsed
  where
    -- fn = take 5
    fn lines =
      let fmt = catMaybes $ map mapper lines
      in take 5 fmt
    mapper []            = Nothing
    mapper (word:sounds) = Just (sounds,word)
