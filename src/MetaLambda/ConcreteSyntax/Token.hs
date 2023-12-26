module MetaLambda.ConcreteSyntax.Token where

import           Text.Megaparsec.Pos

data Tok
  = Dot
  | Comma
  | Bar
  | Colon
  | Equal
  | Eq
  | Le
  | Lt
  | Plus
  | Minus
  | Prod
  | Div
  | Mod
  | Pow
  | Cons
  | Arrow
  | RTack
  | BrkL
  | BrkR
  | ParL
  | ParR
  | Fn
  | Fix
  | Box
  | Let
  | In
  | Match
  | With
  | End
  | If
  | Then
  | Else
  | Base
  | Bool
  | Int
  | List
  | True
  | False
  | Of
  | Inject
  | Ident String
  | Num Integer
  deriving (Eq, Show)

data Token = Token SourcePos Tok -- deriving Show
instance Show Token where
  show (Token _ t) = show t
