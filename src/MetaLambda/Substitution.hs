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

fromTerm :: Mode mo => mo -> Term mo -> mo -> AllocId
-- m <= n
fromTerm m (Var y)                n | m == n    = singleton y
                                    | otherwise = mempty
fromTerm m (Lam y a t)            n             = fromTerm m t n
fromTerm m (App t1 t2)            n             = fromTerm m t1 n <> fromTerm m t2 n
fromTerm m (Lift m' ctx t)        n             = fromTerm m' t n
fromTerm m (Unlift m' t s)        n | m == n    = foldMap (\t -> fromTerm m t n) s
                                    | otherwise = foldMap (\t -> fromTerm m t n) s <> fromTerm m' t n
fromTerm m (Return m' t)          n | m == n    = mempty
                                    | otherwise = fromTerm m' t n
fromTerm m (LetReturn m' u t1 t2) n          = fromTerm m t1 n <> fromTerm m t2 n

-- substitutions
subst :: Mode mo => mo -> (Id, Term mo) -> mo -> Term mo -> Term mo
subst m xt = ssubst m [xt]

ssubst :: Mode mo => mo -> [(Id, Term mo)] -> mo -> Term mo -> Term mo
ssubst = \m s n t -> go m (Map.fromList s) n t
  where
    -- m >= n
    go :: Mode mo => mo -> Map Id (Term mo) -> mo -> Term mo -> Term mo
    go m s n t@(Var x)              | m == n    = Map.findWithDefault t x s
                                    | otherwise = t
    go m s n (Lam x a t)            | m == n    = Lam x' a (go m s' n t)
                                    | otherwise = Lam x  a (go m s  n t)
      where
        x' = newId (foldMap (\t -> fromTerm m t m) s) x
        s' = Map.insert x (Var x') s
    go m s n (App t1 t2)                        = App (go m s n t1) (go m s n t2)
    go m s n (Lift n' ctx t)                    = Lift n' ctx (go m s n' t)
    go m s n (Unlift n' t s')       | m == n    = Unlift n' t             (go m s n <$> s')
                                    | otherwise = Unlift n' (go m s n' t) (go m s n <$> s')
    go m s n tt@(Return n' t)       | m == n    = tt
                                    | otherwise = Return n' (go m s n' t)
    go m s n (LetReturn n' u t1 t2) | m == n'   = LetReturn n' u' (go m s n t1) (go m s' n t2)
                                    | otherwise = LetReturn n' u  (go m s n t1) (go m s  n t2)
      where
        u' = newId (foldMap (\t -> fromTerm m t m) s) u
        s' = Map.insert u (Var u') s
