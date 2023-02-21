module Main (main) where

import ELang.Tokenizer (tokenize)

main :: IO ()
main =
  readFile "examples/addition_fn.elang" >>= handle
  where
    handle :: String -> IO ()
    handle contents = do
      let tokenized = tokenize contents
      print tokenized
