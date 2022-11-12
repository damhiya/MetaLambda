module MetaLambda.Evaluation where

import           MetaLambda.Substitution
import           MetaLambda.Syntax

invalid :: Term
invalid = error "not type checked"

-- call by name evaluation
eval :: Term -> Term
eval (Var x) = invalid
eval t@(Lam _ _ _) = t
eval (App t1 t2) =
  case eval t1 of
    Lam x _ t1' -> eval (ssubst 0 [(x, t2)] t1')
    _           -> invalid
eval t@(Lift _ _) = t
eval (Unlift t s) =
  case eval t of
    Lift ctx t' -> eval (ssubst 0 (zip (map fst ctx) s) t')
    _           -> invalid
eval t@(Return _) = t
eval (LetReturn u t1 t2) =
  case eval t1 of
    Return t1' -> eval (ssubst 1 [(u, t1')] t2)
    _          -> invalid
