module MetaLambda.Syntax where

import           Data.MapList
import           Numeric.Natural

-- identifier
data Id = Id !String !Natural deriving (Show, Eq, Ord)

-- mode, type and context
class Eq mo => Mode mo where
  dependencyOf :: mo -> mo -> Bool
  hasUpshift :: mo -> mo -> Bool
  hasDownshift :: mo -> mo -> Bool
  hasArr :: mo -> Bool
  hasBase :: mo -> Bool

type StdMode = Natural

instance Mode StdMode where
  dependencyOf m n = m <= n
  hasUpshift m n = m+1 == n
  hasDownshift m n = m+1 == n
  hasArr m = True
  hasBase m = True

data Type mo where
  -- Upshift m Ψ A = (Ψ ⊢ₘ A)
  Upshift :: mo -> Ctxs mo -> Type mo -> Type mo
  -- Downshift m A = ↓ᵐ A
  Downshift :: mo -> Type mo -> Type mo
  Arr :: Type mo -> Type mo -> Type mo
  Base :: Type mo
  deriving (Eq, Show)

type Ctx mo = [(Id, Type mo)]
type Ctxs mo = MapList mo (Id, Type mo)

-- term and substitution
data Term mo
  = Var Id
  | Lam Id (Type mo) (Term mo)
  | App (Term mo) (Term mo)
  | Lift mo (Ctxs mo) (Term mo)
  | Unlift mo (Term mo) (Substs mo)
  | Return mo (Term mo)
  | LetReturn mo Id (Term mo) (Term mo)
  deriving Show

type Subst mo = [Term mo]
type Substs mo = MapList mo (Term mo)
