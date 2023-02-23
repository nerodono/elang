module ELang.Parser
( parseTokens )
where

import ELang.Token
import ELang.Expr (Expr(..), sortByPrecedences)
import ELang.Utils ( mapFst )
import ELang.PredicateStack

import Data.Maybe (fromMaybe)
import Prelude hiding (until)

unwrapMaybe :: Maybe a -> a
unwrapMaybe = fromMaybe $ error "Unwrapped Nothing value"

parseTokens :: [Token] -> Maybe Expr
parseTokens [] = Nothing
parseTokens tokens =
  fmap sortByPrecedences $ fst $ parse Nothing emptyStack tokens

  where
    parse :: Maybe Expr -> PredicateStack
                        -> [Token]
                        -> (Maybe Expr, [Token])
    parse maybeLhs stack' (currentToken:tokTail) =
      let (boolVal, stack) = testPredicate currentToken in
        if not boolVal then

          case currentToken of
            Bracket { isOpen = True } ->
              let bracketsStack = pushToTop stack isClosingBracket
                  (inBracketsExpr, tail') = mapFst unwrapMaybe $ parse Nothing bracketsStack tokTail
              in
                parse (Just inBracketsExpr) stack tail'

            Keyword keyword ->
              case keyword of
                If ->
                  let untilThenStack   = pushToTop stack (isKeyword Then)
                      untilElseStack   = pushToTop stack (isKeyword Else)
                      (ifCond, ifTail) = mapFst unwrapMaybe $ parse Nothing untilThenStack tokTail
                      (true', falseT') = mapFst unwrapMaybe $ parse Nothing untilElseStack ifTail
                      (false', tail')  = mapFst unwrapMaybe $ parse Nothing stack falseT'
                  in
                    result tail' $ ExprIf { ifCond
                                          , trueExpr = true'
                                          , falseExpr = false'
                                          }
                Let ->
                  case tokTail of
                    (Ident name : Op Equal : opTail) ->
                      let untilInKw = pushToTop stack (isKeyword In)
                          (bindExpr, boundToTokens) = mapFst unwrapMaybe $ parse Nothing untilInKw opTail
                          (contextExpr, tail') = mapFst unwrapMaybe $ parse Nothing stack boundToTokens
                      in
                        result tail' $ ExprPlainLet { binding = name
                                                    , bindExpr
                                                    , contextExpr
                                                    }
                    (Ident name : Ident argument : funcTail) ->
                      undefined
                    t ->
                      error $ "Malformed let in expression, tail: " ++ show t
                _ -> error $ "Unexpected keyword " ++ show keyword ++ ", tail: " ++ show tokTail

            Ident ident ->
              parse (Just $ ExprIdent ident) stack' tokTail
            Lit lit ->
              parse (Just $ ExprLit lit) stack' tokTail
            Op operator ->
              case maybeLhs of
                Just lhs ->
                  let (rhs, tail') = mapFst unwrapMaybe $ parse Nothing stack tokTail
                  in
                    result tail' $ Binary lhs rhs operator
                Nothing ->
                  -- TODO: Unary operations
                  undefined

            _ -> undefined

        else
          (maybeLhs, tokTail)
      where
        -- Utilities

        result :: [Token] -> Expr -> (Maybe Expr, [Token])
        result tail' expr =
          (Just expr, tail')

        isKeyword :: Keyword -> Token -> Bool
        isKeyword keyword token =
          case token of
            Keyword keyword' -> keyword == keyword'
            _ -> False

        isClosingBracket :: Token -> Bool
        isClosingBracket Bracket { isOpen = False } = True
        isClosingBracket _ = False

        -- Predicate testing
        testPredicate :: Token -> (Bool, PredicateStack)
        testPredicate token =
          case tryPop stack' of
            Just (predicate, pTail) ->
              if predicate token then
                (True, pTail)
              else
                (False, stack')
            Nothing -> (False, stack')

    parse maybeLhs _ [] = (maybeLhs, [])
