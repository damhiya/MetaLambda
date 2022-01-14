{-# LANGUAGE OverloadedStrings #-}

module Parser.Lexer where

import Data.Functor
import Data.Text
import Data.Void
import Control.Applicative
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P

import Parser.Syntax

type PM = P.Parsec Void Text

token :: PM Token
token = do
  pos <- P.getSourcePos
  tok <- P.choice
    [ P.string "." $> TDot
    , P.string "," $> TComma
    , P.string ":" $> TColon
    , P.string "=" $> TEqual
    , P.string "->" $> TArrow
    , P.string "|-" $> TVDash
    , P.string "[" $> TBrkL
    , P.string "]" $> TBrkR
    , P.string "(" $> TParL
    , P.string ")" $> TParR
    , do
        x <- headChar
        xs <- many tailChar
        pure $ case x:xs of
          "fn"   -> TFn
          "box"  -> TBox
          "let"  -> TLet
          "in"   -> TIn
          "with" -> TWith
          "base" -> TBase
          ident  -> TIdent ident
    ]
  P.space
  pure (Token pos tok)
  where
    headChar = P.letterChar <|> P.char '_'
    tailChar = P.alphaNumChar <|> P.char '_'

tokens :: PM [Token]
tokens = P.space *> many token <* P.eof

tokenize :: String -> Text -> Either (P.ParseErrorBundle Text Void) [Token]
tokenize = P.parse tokens
