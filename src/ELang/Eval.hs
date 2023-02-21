module ELang.Eval
( Expr(..)
, eval
, EvalResult(..) )
where

import ELang.Expr ( EvalResult(..), Expr(..) )
import ELang.Token ( Lit(..), Operator(..) )
import ELang.BindContext

eval :: BindContext -> Expr -> EvalResult
eval _ (ExprLit lit) =
  case lit of
    IntLit  i -> IntResult i
    StrLit  s -> StrResult s
    BoolLit b -> BoolResult b

eval ctx (ExprIdent ident) =
  case findItemByName ctx ident of
    Just item ->
      case item of
        Function { fnBody, fnArgs } ->
          -- TODO: Implement function application
          undefined
        NameBinding { value } ->
          eval ctx value
    Nothing   -> error $ "Undefined binding " ++ ident

eval ctx ExprPlainLet { binding, bindExpr, contextExpr } =
  let contextItem = NameBinding { value = bindExpr }
      newBinding  = Binding { name = binding, item = contextItem }
      newContext  = withNewBinding ctx newBinding
  in
    eval newContext contextExpr

eval ctx ExprIf { ifCond, trueExpr, falseExpr } =
  case eval ctx ifCond of
    BoolResult True  -> true
    BoolResult False -> false
    StrResult  s     -> if null s then false else true
    IntResult  i     -> if i == 0 then false else true
  where
    true = eval ctx trueExpr
    false = eval ctx falseExpr

eval ctx (Unary lhs op) =
  let fn = case op of
            Add -> id
            Sub -> neg
            _   -> undefined
  in
    fn (eval ctx lhs)
  where
    neg :: EvalResult -> EvalResult
    neg (BoolResult bLhs) =
      -- Logical not
      BoolResult $ not bLhs
    neg (IntResult iLhs) =
      IntResult $ -iLhs
    neg _ = undefined

eval ctx (Binary lhs rhs op) =
  let fn = case op of
            Add    -> add
            Sub    -> sub
            Mul    -> mul
            Div    -> fdiv
            Equal  -> equal
  in
    fn (eval ctx lhs) (eval ctx rhs)
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

eval _ _ = undefined
