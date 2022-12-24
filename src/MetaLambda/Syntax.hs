module MetaLambda.Syntax where

import Numeric.Natural
import Data.Map
import Data.List.NonEmpty

-- identifier
data Id = Id !String !Natural deriving (Show, Eq, Ord)

-- mode, type and context
class Eq mo => Mode mo where
  globalCtxOf :: mo -> mo -> Bool
  hasUpshift :: mo -> mo -> Bool
  hasDownshift :: mo -> mo -> Bool
  hasArr :: mo -> Bool
  hasBase :: mo -> Bool

instance Mode StdMode where
  globalCtxOf m n = m <= n
  hasUpshift m n = m+1 == n
  hasDownshift m n = m+1 == n
  hasArr m = True
  hasBase m = True

type StdMode = Natural

data Type mo where
  -- Upshift m Γ A = (Γ ⊢ₘ A)
  Upshift :: mo -> Ctx mo -> Type mo -> Type mo
  -- Downshift m A = ↓ᵐ A
  Downshift :: mo -> Type mo -> Type mo
  Arr :: Type mo -> Type mo -> Type mo
  Base :: Type mo
  deriving (Eq, Show)

type Ctx mo = [(Id, Type mo)]
type Ctxs mo = Map mo (NonEmpty (Id, Type mo))

-- term and substitution
data Term mo
  = Var Id
  | Lam Id (Type mo) (Term mo)
  | App (Term mo) (Term mo)
  | Lift mo (Ctx mo) (Term mo)
  | Unlift mo (Term mo) (Subst mo)
  | Return mo (Term mo)
  | LetReturn mo Id (Term mo) (Term mo)
  deriving Show

type Subst mo = [Term mo]
