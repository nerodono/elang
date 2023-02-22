module Main (main) where

import ELang.Tokenizer (tokenize)
import ELang.Parser (parseTokens)

main :: IO ()
main =
  readFile "examples/test.elang" >>= handle
  where
    handle :: String -> IO ()
    handle contents = do
      let tokenized = tokenize contents
      print tokenized
      let parsed = parseTokens tokenized
      print parsed
