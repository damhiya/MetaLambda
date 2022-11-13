module MetaLambda.Evaluation where

import           MetaLambda.Substitution
import           MetaLambda.Syntax

invalid :: Term
invalid = error "not type checked"

-- call by name evaluation
eval :: Mode -> Term -> Term
eval m (Var x) = invalid
eval m t@(Lam _ _ _) = t
eval m (App t1 t2) =
  case eval m t1 of
    Lam x _ t1' -> eval m (ssubst m [(x, t2)] m t1')
    _           -> invalid
eval m t@(Lift _ _) = t
eval m (Unlift t s) =
  case eval (m+1) t of
    Lift ctx t' -> eval m (ssubst m (zip (map fst ctx) s) m t')
    _           -> invalid
eval m t@(Return _) = t
eval m (LetReturn u t1 t2) =
  case eval m t1 of
    Return t1' -> eval m (ssubst (m+1) [(u, t1')] m t2)
    _          -> invalid
