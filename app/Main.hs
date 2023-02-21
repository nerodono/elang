module Main (main) where

import Token

main :: IO ()
main = do print $ [
                    Ident "HelloWorld",
                    Lit $ StrLit "Nero"
                  ]
