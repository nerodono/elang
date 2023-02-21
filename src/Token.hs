module Token
( Lit(..)
, Token(..)
, Operator(..)
, Keyword(..)
, precedenceOf
)
where

class Precedence obj where
  precedenceOf :: obj -> Int

data Keyword = If
             | Else
             | Then
             deriving(Show)

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
           deriving(Show)

instance Precedence Operator where
  precedenceOf operator =
    case operator of
      Add    -> 1
      Sub    -> 1
      Mul    -> 2
      Div    -> 2
      Equal  -> 1

