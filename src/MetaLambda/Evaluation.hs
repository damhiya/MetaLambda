module MetaLambda.Evaluation where

import           MetaLambda.Substitution
import           MetaLambda.Syntax

-- call by name evaluation
(//) :: [Term] -> Ctx -> [(Id, Term)]
(//) s ctx = zip (map fst ctx) s

splice, evaluate :: Mode -> Term -> Term
splice = \m t -> go m m t
  where
    go :: Mode -> Mode -> Term -> Term
    go m n t = case t of
      Var x                   -> t
      Lam x a t1              -> Lam x a (go m n t1)
      App t1 t2               -> App (go m n t1) (go m n t2)
      Lift ctx t1             -> Lift ctx (go m (n-1) t1)
      Unlift t1 s | m < n+1   -> case evaluate (n+1) t1 of
                                   Lift ctx t1' -> ssubst m ((go m n <$> s) // ctx) n t1'
                                   t1'          -> Unlift t1' (go m n <$> s)
                  | otherwise -> Unlift (go m (n+1) t1) (go m n <$> s)
      Return t1   | m < n+1   -> Return (evaluate (n+1) t1)
                  | otherwise -> Return (go m (n+1) t1)
      LetReturn u t1 t2       -> LetReturn u (go m n t1) (go m n t2)
evaluate = \m t -> go m (splice m t)
  where
    go :: Mode -> Term -> Term
    go m t = case t of
      Var x             -> t
      Lam x a t1        -> t
      App t1 t2         -> case go m t1 of
                             Lam x a t1' -> go m (subst m (x, t1') m t2)
                             t1'         -> App t1' t2
      Lift ctx t1       -> Lift ctx (splice (m-1) t1)
      Unlift t1 s       -> t
      Return t1         -> t
      LetReturn u t1 t2 -> case go m t1 of
                             Return t1' -> evaluate m (subst (m+1) (u, t1') m t2)
                             t1'        -> LetReturn u t1' t2
