module ELang.Parser
( parseTokens )
where

import ELang.Eval (Expr(..))
import ELang.Token
import ELang.Utils ( mapSnd )

parseTokens :: [Token] -> Maybe Expr
parseTokens [] = undefined
parseTokens (token:tTail) =
  case token of
    Keyword kw -> Just $
      ( case kw of
         If  -> parseIf
         Let -> parseLet
         _ -> undefined ) tTail
    _ -> undefined
  where
    -- If
    parseIf :: [Token] -> Expr
    parseIf [] = error "Empty if branch"
    parseIf (token:ifTail) = undefined

    -- Let
    parseLet :: [Token] -> Expr
    parseLet [] = error "Empty let binding"
    parseLet (token:letTail) =
      case token of
        Ident ident ->
          case letTail of
            (Op Equal : bindingTail) ->
              let (tokensBefore, tokensAfter) =
                    mapSnd tail $ span isNotKwIn bindingTail
              in undefined
            args -> undefined
        tok         -> error $ "Unexpected token: " ++ show tok ++ ", expected identifier"
      where
        isNotKwIn :: Token -> Bool
        isNotKwIn (Keyword In) = False
        isNotKwIn _            = True


