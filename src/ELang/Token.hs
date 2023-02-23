module ELang.Token
( Lit(..)
, Token(..)
, Operator(..)
, Keyword(..)
, Precedence(..)
)
where

class Precedence obj where
  precedenceOf :: obj -> Int

data Keyword = If
             | Else
             | Stringify
             | Then
             | Let
             | In
             deriving(Show, Eq)

data Operator = Add
              | Sub
              | Mul
              | Div
              | Equal
              deriving(Show)

data Lit = StrLit String
         | IntLit Integer
         | BoolLit Bool
         deriving(Show)

data Token = Ident String
           | Lit Lit
           | Op Operator
           | Keyword Keyword
           | Bracket { isOpen :: Bool }
           deriving(Show)

instance Precedence Operator where
  precedenceOf operator =
    case operator of
      Add    -> 1
      Sub    -> 1
      Mul    -> 2
      Div    -> 2
      Equal  -> 1

