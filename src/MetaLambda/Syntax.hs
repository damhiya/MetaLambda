module MetaLambda.Syntax where

import Numeric.Natural

-- identifier
data Id = Id !String !Natural deriving (Show, Eq, Ord)

-- mode, type and context
type Mode = Natural

data Type where
  Upshift :: Ctx -> Type -> Type
  Downshift :: Type -> Type
  Arr :: Type -> Type -> Type
  Base :: Type
  deriving (Eq, Show)

type Ctx = [(Id, Type)]

-- term and substitution
data Term
  = Var Id
  | Lam Id Type Term
  | App Term Term
  | Lift Ctx Term
  | Unlift Term Subst
  | Return Term
  | LetReturn Id Term Term
  deriving Show

type Subst = [Term]
