module Main (main) where

import LangExpr (eval, Expr(..))
import Tokenizer (tokenize)
import Token

main :: IO ()
main =
  readFile "examples/addition_fn.elang" >>= handle
  where
    handle :: String -> IO ()
    handle contents = do
      let tokenized = tokenize contents
      print tokenized
