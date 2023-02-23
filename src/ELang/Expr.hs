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
                deriving(Eq)

data Expr = Binary Expr Expr Operator
          | ExprStringify Expr
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

instance Show EvalResult where
  show (IntResult  i) = show i
  show (BoolResult b) = show b
  show (StrResult  s) = show s

sortByPrecedences :: Expr -> Expr
sortByPrecedences = id

