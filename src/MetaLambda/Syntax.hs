module MetaLambda.Syntax where

import Numeric.Natural

-- identifier
data Id = Id !String !Natural deriving (Show, Eq, Ord)

-- mode, type and context
type Mode = Natural

data Type where
  -- Upshift m Γ A = (Γ ⊢ₘ A)
  Upshift :: Mode -> Ctx -> Type -> Type
  -- Downshift m A = ↓ᵐ A
  Downshift :: Mode -> Type -> Type
  Arr :: Type -> Type -> Type
  Base :: Type
  deriving (Eq, Show)

type Ctx = [(Id, Type)]

-- term and substitution
data Term
  = Var Id
  | Lam Id Type Term
  | App Term Term
  | Lift Mode Ctx Term
  | Unlift Mode Term Subst
  | Return Mode Term
  | LetReturn Mode Id Term Term
  deriving Show

type Subst = [Term]
