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
      Var x                      -> t
      Lam x a t1                 -> Lam x a (go m n t1)
      App t1 t2                  -> App (go m n t1) (go m n t2)
      Lift n' ctx t1             -> Lift n' ctx (go m n' t1)
      Unlift n' t1 s | m < n'    -> case evaluate n' t1 of
                                      Lift _ ctx t1' -> ssubst m ((go m n <$> s) // ctx) n t1'
                                      t1'          -> Unlift n' t1' (go m n <$> s)
                     | otherwise -> Unlift n' (go m n' t1) (go m n <$> s)
      Return n' t1   | m < n'    -> Return n' (evaluate n' t1)
                     | otherwise -> Return n' (go m n' t1)
      LetReturn n' u t1 t2       -> LetReturn n' u (go m n t1) (go m n t2)
evaluate = \m t -> go m (splice m t)
  where
    go :: Mode -> Term -> Term
    go m t = case t of
      Var x                -> t
      Lam x a t1           -> t
      App t1 t2            -> case go m t1 of
                                Lam x a t1' -> go m (subst m (x, t1') m t2)
                                t1'         -> App t1' t2
      Lift m' ctx t1       -> Lift m' ctx (splice m' t1)
      Unlift m' t1 s       -> t
      Return m' t1         -> t
      LetReturn m' u t1 t2 -> case go m t1 of
                                Return _ t1' -> evaluate m (subst m' (u, t1') m t2)
                                t1'          -> LetReturn m' u t1' t2
