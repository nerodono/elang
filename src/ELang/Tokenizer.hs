module ELang.Tokenizer (tokenize) where

import ELang.Token ( Token(..)
                   , Lit(..)
                   , Operator(..)
                   , Keyword(..)
                   )
import ELang.Utils (mapFst, mapSnd)

tokenize :: String -> [Token]
tokenize "" = []
tokenize (c:sTail)
  | isDigit c = continue $ parseInt original
  | isSkipCharacter c = tokenize sTail
  | isOperator c = continue $ parseOperator original
  | isStrLitStart c = continue $ parseStrLit sTail
  | isIdentChar c = continue $ mapFst postProcessIdent $ parseIdent original
  | isBracket c = continue $ parseBracket original
  | otherwise = undefined
  where
    original = c : sTail

    isBracket :: Char -> Bool
    isBracket = flip elem "()"

    isIdentChar :: Char -> Bool
    isIdentChar = flip elem $
      ['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9'] ++ "_'!"

    isStrLitStart :: Char -> Bool
    isStrLitStart = (== '"')

    isDigit :: Char -> Bool
    isDigit = flip elem ['0'..'9']

    isSkipCharacter :: Char -> Bool
    isSkipCharacter = flip elem " \t\n"

    isOperator :: Char -> Bool
    isOperator = flip elem "+-=/*"

    postProcessIdent :: Token -> Token
    postProcessIdent (Ident strIdent) =
      case strIdent of
        "true"  -> Lit $ BoolLit True
        "false" -> Lit $ BoolLit False
        "if"    -> Keyword If
        "else"  -> Keyword Else
        "then"  -> Keyword Then
        "let"   -> Keyword Let
        "in"    -> Keyword In
        "stringify" -> Keyword Stringify

        ident -> Ident ident
    postProcessIdent _ = undefined

    parseBracket :: String -> (Token, String)
    parseBracket "" = undefined
    parseBracket (x:xs) = (Bracket { isOpen = x == '(' }, xs)

    parseIdent :: String -> (Token, String)
    parseIdent = mapFst Ident . span isIdentChar

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
                        '=' -> Equal
                        _ -> undefined
      in (token, opTail)

    parseInt :: String -> (Token, String)
    parseInt s =
      mapFst (Lit . IntLit . read) $ span isDigit s

    continue :: (Token, String) -> [Token]
    continue (token, remaining) =
      token : tokenize remaining
