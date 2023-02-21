module Main (main) where

import LangExpr (eval, Expr(..))
import Token

main :: IO ()
main = print $ eval ( let literal = ExprLit $ StrLit "Hello "
                          times   = ExprLit $ IntLit 10
                      in
                        Binary literal times Mul )
