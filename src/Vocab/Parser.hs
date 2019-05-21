module Vocab.Parser where

import           Data.Char                     (toUpper)
import           Text.Parsec.ByteString        (parseFromFile)
import           Text.ParserCombinators.Parsec (ParseError, alphaNum, char,
                                                endBy, many, many1, newline,
                                                oneOf, sepBy, (<|>))

-- example data:
-- ABBOUD  AH0 B UW1 D
-- ABBOUD(1)  AH0 B AW1 D

file = endBy line eol
line = sepBy cell (many1 $ char ' ')
cell = many (alphaNum <|> oneOf keepChars <|> oneOf "().")
eol = newline

keepChars = "_-'" ++ ['A' .. 'Z']

parseInput :: String -> IO (Either ParseError [[String]])
parseInput input = do
  parsed <- parseFromFile file input
  return $ fmap (map fn) parsed
  -- remove duplicate signals from the first token (i.e. "WORD(2)" -> "WORD")
  where
    fn []     = []
    fn (x:xs) =
      [x' | x' <- map toUpper x, x' `elem` keepChars]:xs
