module Token
( Lit(..)
, Token(..)
, Operator(..)
)
where


data Operator = Add
              | Sub
              | Mul
              | Div
              | Assign
              deriving(Show)

data Lit = StrLit String
          | IntLit Integer
          deriving(Show)

data Token = Ident String
           | Lit Lit
           | Op Operator
           deriving(Show)
