module Tokenizer (tokenize) where

import Token (Token(..), Lit(..), Operator(..))
import Utils (mapFst, mapSnd)

tokenize :: String -> [Token]
tokenize "" = []
tokenize (c:sTail)
  | isDigit c = continue $ parseInt original
  | isSkipCharacter c = tokenize sTail
  | isOperator c = continue $ parseOperator original
  | isStrLitStart c = continue $ parseStrLit sTail
  | otherwise = undefined
  where
    original = c : sTail

    isStrLitStart :: Char -> Bool
    isStrLitStart = (== '"')

    isDigit :: Char -> Bool
    isDigit = flip elem ['0'..'9']

    isSkipCharacter :: Char -> Bool
    isSkipCharacter = flip elem " \t\n"

    isOperator :: Char -> Bool
    isOperator = flip elem "+-=/*"

    parseStrLit :: String -> (Token, String)
    parseStrLit s = mapSnd tail $ mapFst (Lit . StrLit) $ break isStrLitStart s

    parseOperator :: String -> (Token, String)
    parseOperator "" = undefined
    parseOperator (start:opTail) =
      let token = Op $ case start of
                        '+' -> Add
                        '-' -> Sub
                        '*' -> Mul
                        '/' -> Div
                        '=' -> Assign
                        _ -> undefined
      in (token, opTail)

    parseInt :: String -> (Token, String)
    parseInt s =
      mapFst (Lit . IntLit . read) $ span isDigit s

    continue :: (Token, String) -> [Token]
    continue (token, remaining) =
      token : tokenize remaining
