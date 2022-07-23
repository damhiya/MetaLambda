module Parser.Syntax where

import           Text.Megaparsec.Pos

data Tok
  = TDot
  | TComma
  | TColon
  | TEqual
  | TArrow
  | TVDash
  | TBrkL
  | TBrkR
  | TParL
  | TParR
  | TFn
  | TBox
  | TLet
  | TIn
  | TWith
  | TBase
  | TIdent String
  deriving (Eq, Show)

data Token = Token SourcePos Tok -- deriving Show
instance Show Token where
  show (Token _ t) = show t
