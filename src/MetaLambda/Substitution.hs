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

fromTerm :: Natural -> Term -> AllocId
fromTerm n (Var y)      | n == 0    = singleton y
                        | otherwise = mempty
fromTerm n (Lam y a t)              = fromTerm n t
fromTerm n (App t1 t2)              = fromTerm n t1 <> fromTerm n t2
fromTerm n (Lift ctx t)             = fromTerm (n+1) t
fromTerm n (Unlift t s) | n == 0    = foldMap (fromTerm n) s
                        | otherwise = foldMap (fromTerm n) s <> fromTerm (n-1) t 
fromTerm n (Return t)   | n == 0    = mempty
                        | otherwise = fromTerm (n-1) t
fromTerm n (LetReturn u t1 t2)      = fromTerm n t1 <> fromTerm n t2

-- substitutions
ssubst :: Natural -> [(Id, Term)] -> Term -> Term
ssubst = \n s t -> go n (Map.fromList s) t
  where
    invalid :: Term
    invalid = error "invalid simultaneous substitution"

    go :: Natural -> Map Id Term -> Term -> Term
    go n s t@(Var x)            | n == 0    = Map.findWithDefault invalid x s
                                | otherwise = t
    go n s (Lam x a t)          | n == 0    = Lam x' a (go n s' t)
                                | otherwise = Lam x  a (go n s  t)
      where
        x' = newId (foldMap (fromTerm 0) s) x
        s' = Map.union (Map.singleton x (Var x')) s
    go n s (App t1 t2)                      = App (go n s t1) (go n s t2)
    go n s (Lift ctx t)                     = Lift ctx (go (n+1) s t)
    go n s (Unlift t s')        | n == 0    = Unlift t              (go n s <$> s')
                                | otherwise = Unlift (go (n-1) s t) (go n s <$> s')
    go n s tt@(Return t)        | n == 0    = tt
                                | otherwise = Return (go (n-1) s t)
    go n s (LetReturn u t1 t2)  | n == 1    = LetReturn u' (go n s' t1) (go n s' t2)
                                | otherwise = LetReturn u  (go n s  t1) (go n s  t2)
      where
        u' = newId (foldMap (fromTerm 0) s) u
        s' = Map.union (Map.singleton u (Var u')) s
