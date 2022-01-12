module Reduction.Normalization where

import qualified Syntax.Object as O
import qualified Syntax.Meta as M
import Reduction.Substitution

-- normal order reduction
normalize :: M.Tm -> M.Tm
normalize tm@(M.Var _) = tm
normalize tm@(M.Abs _ _ _) = tm
normalize (M.App tm1 tm2) =
  case normalize tm1 of
    M.Abs x _ tm3 -> normalize (substMeta (x, tm2) tm3)
    tm1' -> M.App tm1' tm2
normalize tm@(M.Box _ _) = tm
normalize (M.LetBox olectx u tm1 tm2) =
  case normalize tm1 of
    M.Box olctx otm -> normalize (substGlobal (u, olectx, otm) tm2)
    tm1' -> M.App tm1' tm2
