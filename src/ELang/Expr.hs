module ELang.Expr
( EvalResult(..)
, Expr(..)
)
where

import ELang.Token ( Operator(..)
                   , Lit(..)
                   )


data EvalResult = IntResult Integer
                | StrResult String
                | BoolResult Bool
                deriving(Show, Eq)

data Expr = Binary Expr Expr Operator
          | ExprIdent String
          | Unary Expr Operator
          | ExprLit Lit
          | ExprPlainLet { binding :: String
                         , bindExpr :: Expr
                         , contextExpr :: Expr
                         }
          | ExprIf { ifCond :: Expr
                   , trueExpr :: Expr
                   , falseExpr :: Expr
                   }
          deriving(Show)
