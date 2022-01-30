module Equality where

import Syntax
import Reduction.Substitution

eqAlpha :: Term -> Term -> Bool
eqAlpha (Var x) (Var x') = x == x'
eqAlpha (Lam x t e) (Lam x' t' e') = t == t' && eqAlpha e (renameVar (x', x) e')
eqAlpha (App e1 e2) (App e1' e2') = eqAlpha e1 e1' && eqAlpha e2 e2'
eqAlpha (Box octx e) (Box octx' e') = octx == octx' && eqAlpha e e'
eqAlpha (LetBox oectx u e1 e2) (LetBox oectx' u' e1' e2')
  = oectx == oectx' && eqAlpha e1 e1' && eqAlpha e2 (renameGVar (u', u) e2')
eqAlpha (Clo u es) (Clo u' es') = u == u' && all (uncurry eqAlpha) (zip es es')
eqAlpha _ _ = False
