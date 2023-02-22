module ELang.Parser
( parseTokens )
where

import ELang.Eval (Expr(..))
import ELang.Token
import ELang.Utils ( mapSnd )

import Data.Maybe (fromMaybe)

unwrapMaybe :: Maybe a -> a
unwrapMaybe = fromMaybe undefined

parseTokens :: [Token] -> Maybe Expr
parseTokens [] = Nothing
parseTokens (token:tTail) =
  case token of
    Op operator -> Just $ parseUnary operator tTail
    -- TODO: Implement non-keyword expression parsing
    Keyword kw -> Just $
      ( case kw of
         If  -> parseIf
         Let -> parseLet
         _ -> undefined ) tTail
    _ -> undefined
  where
    -- Unary operations
    parseUnary :: Operator -> [Token] -> Expr
    parseUnary op (firstToken:tailTokens) =
      case firstToken of
        Lit (IntLit int) -> undefined
        _ -> undefined

    -- If
    parseIf :: [Token] -> Expr
    parseIf ifTokens =
      let (condition, afterThen) = mapSnd tail $ span isNotThen ifTokens
          (ifTrue, ifFalse)      = mapSnd tail $ span isNotElse afterThen
      in
        ExprIf { ifCond    = unwrapMaybe $ parseTokens condition
               , trueExpr  = unwrapMaybe $ parseTokens ifTrue
               , falseExpr = unwrapMaybe $ parseTokens ifFalse
               }
      where
        isNotElse :: Token -> Bool
        isNotElse (Keyword Else) = False
        isNotElse _ = True

        isNotThen :: Token -> Bool
        isNotThen (Keyword Then) = False
        isNotThen _ = True

    -- Let
    parseLet :: [Token] -> Expr
    parseLet [] = error "Empty let binding"
    parseLet (nextToken:letTail) =
      case nextToken of
        Ident ident ->
          case letTail of
            (Op Equal : bindingTail) ->
              let (tokensBefore, tokensAfter) =
                    mapSnd tail $ span isNotKwIn bindingTail
              in
                ExprPlainLet { binding = ident
                             , bindExpr = unwrapMaybe    $ parseTokens tokensBefore
                             , contextExpr = unwrapMaybe $ parseTokens tokensAfter
                             }
            args ->
              -- TODO: Implement function binding
              undefined
        tok         -> error $ "Unexpected token: " ++ show tok ++ ", expected identifier"
      where
        isNotKwIn :: Token -> Bool
        isNotKwIn (Keyword In) = False
        isNotKwIn _            = True
