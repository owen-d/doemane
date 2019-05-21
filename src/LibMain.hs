module LibMain where

import           CLI   (Config (dictionary), runConfig)
import           Vocab (parseInput)


main :: IO ()
main = runConfig run

run :: Config -> IO ()
run conf = do
  parsed <- parseInput . dictionary $ conf
  print $ fmap (take 5) parsed

