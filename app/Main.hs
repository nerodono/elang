module Main (main) where

import ELang.Tokenizer (tokenize)
import ELang.Parser (parseTokens)
import ELang.Eval (eval)
import ELang.BindContext

import Data.Maybe (fromMaybe)



main :: IO ()
main =
  readFile "examples/test.elang" >>= handle
  where
    handle :: String -> IO ()
    handle contents = do
      let tokenized = tokenize contents
      print tokenized
      let parsed = fromMaybe undefined $ parseTokens tokenized
      let evaluated = eval emptyCtx parsed

      putStrLn "Tree:"
      print parsed

      putStrLn "Result:"
      print evaluated
