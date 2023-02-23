module ELang.PredicateStack
( PredicateStack(..)
, emptyStack
, pushToTop
, tryPop
)
where

import ELang.Token (Token)

type UntilPredicate    = Token -> Bool
newtype PredicateStack = PredicateStack [UntilPredicate]

pushToTop :: PredicateStack -> UntilPredicate -> PredicateStack
pushToTop (PredicateStack list) predicate =
  PredicateStack $ predicate : list

tryPop :: PredicateStack -> Maybe (UntilPredicate, PredicateStack)
tryPop (PredicateStack list) =
  case list of
    (top:remaining) -> Just (top, PredicateStack remaining)
    [] -> Nothing

emptyStack :: PredicateStack
emptyStack = PredicateStack []
