module Vocab.Parser where

import           Data.Char                     (toUpper)
import           Text.ParserCombinators.Parsec (ParseError, alphaNum, char,
                                                endBy, many, many1, newline,
                                                oneOf, parse, sepBy, (<|>))

-- example data:
-- ABBOUD  AH0 B UW1 D
-- ABBOUD(1)  AH0 B AW1 D

file = endBy line eol
line = sepBy cell (many1 $ char ' ')
cell = many (alphaNum <|> oneOf "()")
eol = newline

parseInput :: String -> Either ParseError [[String]]
parseInput input =
  fmap (map fn) $ parse file "(unknown)" input
  -- remove duplicate signals from the first token (i.e. "WORD(2)" -> "WORD")
  where
    fn []     = []
    fn (x:xs) =
      [x' | x' <- map toUpper x, x' `elem` ['A' .. 'Z']]:xs
