module MetaLambda.Evaluation where

import           MetaLambda.Substitution
import           MetaLambda.Syntax

invalid :: Term
invalid = error "not type checked"

-- call by name evaluation
eval :: Term -> Term
eval (Var _) = invalid
eval e@(Lam _ _ _) = e
eval (App e1 e2) =
  case eval e1 of
    Lam x _ e3 -> eval (subst (x, e2) e3)
    _          -> invalid
eval e@(Box _ _) = e
eval (LetBox oectx u e1 e2) =
  case eval e1 of
    Box octx oe -> eval (substGlobal (u, oectx, oe) e2)
    _           -> invalid
eval (Clo _ _) = invalid
