module ELang.Expr
( EvalResult(..)
, Expr(..)
, sortByPrecedences
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
          | BracketsExpr Expr
          | ExprIdent String
          | Unary Expr Operator
          | ExprLit Lit
          | ExprPlainLet { binding :: String
                         , bindExpr :: Expr
                         , contextExpr :: Expr
                         }
          | ExprFuncBind { fnName :: String
                         , bodyExpr :: Expr
                         , contextExpr :: Expr
                         , arguments :: [String]
                         }
          | ExprIf { ifCond :: Expr
                   , trueExpr :: Expr
                   , falseExpr :: Expr
                   }
          deriving(Show)

sortByPrecedences :: Expr -> Expr
sortByPrecedences = id

