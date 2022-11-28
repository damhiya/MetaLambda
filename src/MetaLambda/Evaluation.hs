module MetaLambda.Evaluation where

import           MetaLambda.Substitution
import           MetaLambda.Syntax

-- call by name evaluation
eval :: Mode -> Term -> Term
eval = \m t -> go m t m
  where
    invalid :: Term
    invalid = error "not type checked"

    -- m <= n
    go :: Mode -> Term -> Mode -> Term
    go m tt@(Var _)          n | m == n    = invalid
                               | otherwise = tt
    go m tt@(Lam x a t)      n | m == n    = tt
                               | otherwise = Lam x a (go m t n)
    go m (App t1 t2)         n | m == n    =
      case go m t1 n of
        Lam x _ t1' -> go m (ssubst m [(x, t2)] m t1') n
        _           -> invalid
                               | otherwise = App (go m t1 n) (go m t2 n)
    go m (Lift ctx t)        n             = Lift ctx (go (m-1) t n)
    go m (Unlift t s)        n | m == n    =
      case go (m+1) t (n+1) of
        Lift ctx t' -> go m (ssubst m (zip (map fst ctx) s) m t') n
        _           -> invalid
                               | otherwise = undefined
    go m t@(Return _)        n | m == n = t
    go m (LetReturn u t1 t2) n | m == n =
      case go m t1 n of
        Return t1' -> go m (ssubst (m+1) [(u, t1')] m t2) n
        _          -> invalid
