module ELang.BindContext
( ContextItem(..)
, BindContext(..)
, Binding(..)
, ToBinding(..)
, ctxFromList
, findItemByName
, emptyCtx
, extractNameFromBinding
, withNewBinding
)
where

import ELang.Expr (Expr(..))

data ContextItem = Function { fnBody :: Expr
                            , fnArgs :: [String]
                            }
                 | NameBinding { value :: Expr }
                 deriving(Show)

data Binding = Binding { name :: String
                       , item :: ContextItem
                       }
newtype BindContext = BindContext [Binding]

class ToBinding a where
  convertToBinding :: a -> Binding

instance ToBinding Binding where
  convertToBinding = id

-- TODO: Possibly more bindings?

ctxFromList :: (ToBinding a) => [a] -> BindContext
ctxFromList = BindContext . map convertToBinding

emptyCtx :: BindContext
emptyCtx = ctxFromList ([] :: [Binding])

withNewBinding :: BindContext -> Binding -> BindContext
withNewBinding (BindContext bindings) Binding { name, item } =
  let withoutPrev = filter ((/= name) . extractNameFromBinding) bindings
      withNew     = Binding { name, item } : withoutPrev
  in
    BindContext withNew

extractNameFromBinding :: Binding -> String
extractNameFromBinding Binding { name } =
  name

findItemByName :: BindContext -> String -> Maybe ContextItem
findItemByName (BindContext list) name =
  let matched = take 1 $ filter ((== name) . extractName) list
  in
    case matched of
      [] -> Nothing
      [Binding { name = _, item }] -> Just item
      _ -> undefined
  where
    extractName :: Binding -> String
    extractName Binding { name = eName, item = _ } = eName
