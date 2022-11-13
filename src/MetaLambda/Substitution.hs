module MetaLambda.Substitution where

import           Data.Map       (Map)
import qualified Data.Map       as Map
import           Data.Semigroup
import Numeric.Natural

import           MetaLambda.Syntax

-- trace used variable names
type AllocId = String -> Maybe (Max Natural)

newId :: AllocId -> Id -> Id
newId f (Id x i) = Id x (maybe i (+1) (getMax <$> f x))

singleton :: Id -> AllocId
singleton (Id x i) y
  | x == y    = Just (Max i)
  | otherwise = Nothing

fromTerm :: Mode -> Term -> Mode -> AllocId
-- m <= n
fromTerm m (Var y)             n | m == n    = singleton y
                                 | otherwise = mempty
fromTerm m (Lam y a t)         n             = fromTerm m t n
fromTerm m (App t1 t2)         n             = fromTerm m t1 n <> fromTerm m t2 n
fromTerm m (Lift ctx t)        n             = fromTerm (m-1) t n
fromTerm m (Unlift t s)        n | m == n    = foldMap (\t -> fromTerm m t n) s
                                 | otherwise = foldMap (\t -> fromTerm m t n) s <> fromTerm (m+1) t n
fromTerm m (Return t)          n | m == n    = mempty
                                 | otherwise = fromTerm (m+1) t n
fromTerm m (LetReturn u t1 t2) n             = fromTerm m t1 n <> fromTerm m t2 n

-- simultaneous substitutions
ssubst :: Mode -> [(Id, Term)] -> Mode -> Term -> Term
ssubst = \m s n t -> go m (Map.fromList s) n t
  where
    invalid :: Term
    invalid = error "invalid simultaneous substitution"

    -- m >= n
    go :: Mode -> Map Id Term -> Mode -> Term -> Term
    go m s n t@(Var x)           | m == n    = Map.findWithDefault invalid x s
                                 | otherwise = t
    go m s n (Lam x a t)         | m == n    = Lam x' a (go m s' n t)
                                 | otherwise = Lam x  a (go m s  n t)
      where
        x' = newId (foldMap (\t -> fromTerm m t m) s) x
        s' = Map.union (Map.singleton x (Var x')) s
    go m s n (App t1 t2)                     = App (go m s n t1) (go m s n t2)
    go m s n (Lift ctx t)                    = Lift ctx (go m s (n-1) t)
    go m s n (Unlift t s')       | m == n    = Unlift t                (go m s n <$> s')
                                 | otherwise = Unlift (go m s (n+1) t) (go m s n <$> s')
    go m s n tt@(Return t)       | m == n    = tt
                                 | otherwise = Return (go m s (n+1) t)
    go m s n (LetReturn u t1 t2) | m == n+1  = LetReturn u' (go m s' n t1) (go m s' n t2)
                                 | otherwise = LetReturn u  (go m s  n t1) (go m s  n t2)
      where
        u' = newId (foldMap (\t -> fromTerm m t m) s) u
        s' = Map.union (Map.singleton u (Var u')) s
