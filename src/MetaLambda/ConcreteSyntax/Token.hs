module MetaLambda.ConcreteSyntax.Token where

import           Text.Megaparsec.Pos

data Tok
  = TDot
  | TComma
  | TBar
  | TColon
  | TEqual
  | TEq
  | TLe
  | TPlus
  | TMinus
  | TProd
  | TDiv
  | TMod
  | TPow
  | TCons
  | TArrow
  | TRTack
  | TBrkL
  | TBrkR
  | TParL
  | TParR
  | TFn
  | TFix
  | TBox
  | TLet
  | TIn
  | TMatch
  | TWith
  | TEnd
  | TBase
  | TBool
  | TInt
  | TList
  | TTrue
  | TFalse
  | TOf
  | TInject
  | TIdent String
  | TIntLit Integer
  deriving (Eq, Show)

data Token = Token SourcePos Tok -- deriving Show
instance Show Token where
  show (Token _ t) = show t
