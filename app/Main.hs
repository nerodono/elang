module Main (main) where

import LangExpr (eval, Expr(..))
import Token

main :: IO ()
main = print $ eval $ ExprIf { ifCond = Binary (Binary ( ExprLit $ StrLit "Hello " )
                                                ( ExprLit $ StrLit "World" ) Add ) (ExprLit $ StrLit "Hello World")
                                                Equal
                             , trueExpr = ExprLit $ IntLit 1
                             , falseExpr = ExprLit $ IntLit 0
                             }
