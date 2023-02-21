{-# OPTIONS_GHC -Wno-partial-fields #-}
{-# LANGUAGE NamedFieldPuns #-}

module LangExpr
( Expr(..)
, eval
, EvalResult(..) )
where

import Token ( Operator(..)
             , Lit(..)
             )

data EvalResult = IntResult Integer
                | StrResult String
                | BoolResult Bool
                deriving(Show, Eq)

data Expr = Binary Expr Expr Operator
          | Unary Expr Operator
          | ExprLit Lit
          | ExprIf { ifCond :: Expr
                   , trueExpr :: Expr
                   , falseExpr :: Expr
                   }
          deriving(Show)

eval :: Expr -> EvalResult
eval (ExprLit lit) =
  case lit of
    IntLit  i -> IntResult i
    StrLit  s -> StrResult s
    BoolLit b -> BoolResult b

eval ExprIf { ifCond, trueExpr, falseExpr } =
  case eval ifCond of
    BoolResult True  -> true
    BoolResult False -> false
    StrResult  s     -> if null s then false else true
    IntResult  i     -> if i == 0 then false else true
  where
    true = eval trueExpr
    false = eval falseExpr

eval (Unary lhs op) =
  let fn = case op of
            Add -> id
            Sub -> neg
            _   -> undefined
  in
    fn (eval lhs)
  where
    neg :: EvalResult -> EvalResult
    neg (BoolResult bLhs) =
      -- Logical not
      BoolResult $ not bLhs
    neg (IntResult iLhs) =
      IntResult $ -iLhs
    neg _ = undefined

eval (Binary lhs rhs op) =
  let fn = case op of
            Add    -> add
            Sub    -> sub
            Mul    -> mul
            Div    -> fdiv
            Equal  -> equal
  in
    fn (eval lhs) (eval rhs)
  where
    -- equal
    equal :: EvalResult -> EvalResult -> EvalResult
    equal anyLhs = BoolResult . (anyLhs ==)

    -- Div
    fdiv :: EvalResult -> EvalResult -> EvalResult
    fdiv (IntResult iLhs) (IntResult iRhs) =
      IntResult $ iLhs `div` iRhs
    fdiv _ _ = undefined

    -- Mul
    mul :: EvalResult -> EvalResult -> EvalResult
    mul (IntResult iLhs) (IntResult iRhs) =
      IntResult $ iLhs * iRhs
    mul (StrResult sLhs) (IntResult iRhs) =
      -- String repetition
      StrResult $ concat $ replicate (fromIntegral iRhs) sLhs
    mul (BoolResult bLhs) (BoolResult bRhs) =
      -- Logical multiplication
      BoolResult $ bLhs && bRhs
    mul _ _ = undefined

    -- Sub
    sub :: EvalResult -> EvalResult -> EvalResult
    sub (IntResult iLhs) (IntResult iRhs) =
      IntResult $ iLhs + iRhs
    sub _ _ = undefined

    -- Add
    add :: EvalResult -> EvalResult -> EvalResult
    add (StrResult sLhs) (StrResult sRhs) =
      StrResult $ sLhs ++ sRhs
    add (IntResult iLhs) (IntResult iRhs) =
      IntResult $ iLhs + iRhs
    add (BoolResult bLhs) (BoolResult bRhs) =
      -- Logical addition
      BoolResult $ bLhs || bRhs
    add _ _ = undefined

eval _ = undefined
