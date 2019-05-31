{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Repl where

import           Data.Aeson                 (FromJSON, ToJSON, encode)
import qualified Data.ByteString.Lazy.Char8 as CL
import qualified Data.Char                  as Char
import           Data.List                  ((\\))
import           Data.Map.Strict            (Map)
import qualified Data.Map.Strict            as Map
import           Data.Maybe                 (catMaybes, fromMaybe)
import           Data.String                (words)
import           GHC.Generics               (Generic)
import           PTree                      (PTree)
import qualified PTree
import qualified System.IO                  as IO

data Result =
  Result
    { sounds  :: [String]
    , matches :: [[String]]
    , sources :: [String]
    }
  deriving (Show, Generic)

instance FromJSON Result
instance ToJSON Result

allowed = ['A'..'Z'] ++ [' ']

prompt :: IO [String]
prompt = do
  IO.putStrLn "Enter Input (space separated words):"
  line <- IO.getLine
  let ws = words $ (filter $ \x -> x `elem` allowed) $ map Char.toUpper line
  return ws

eval :: Map String [String] -> PTree String String -> [String] -> [[String]]
eval cache tree ws =
  let sounds = concat . catMaybes $ map lookupWord ws
  in (fromMaybe [] (PTree.find tree sounds)) \\ [ws]
  where
    lookupWord w = Map.lookup w cache

display :: String -> [[String]] -> IO ()
display src results = do
  IO.putStrLn $ src <> " -> " <> (CL.unpack . encode $ results)

run :: Map [Char] [String] -> PTree String String -> IO ()
run map tree = do
  ws <- prompt
  let matches = eval map tree ws
  display (unwords ws) matches
  run map tree
