module Reduction.Evaluation where

import           Reduction.Substitution
import           Syntax

invalid :: Term
invalid = error "not type checked"

-- call by name evaluation
eval :: Term -> Term
eval (Var _) = invalid
eval e@(Lam _ _ _) = e
eval (App e1 e2) =
  case eval e1 of
    Lam x _ e3 -> eval (subst (x, e2) e3)
    e1'        -> invalid
eval e@(Box _ _) = e
eval (LetBox oectx u e1 e2) =
  case eval e1 of
    Box octx oe -> eval (substGlobal (u, oectx, oe) e2)
    e1'         -> invalid
eval (Clo u es) = invalid
