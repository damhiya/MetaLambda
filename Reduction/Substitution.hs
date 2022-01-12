module Reduction.Substitution where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Semigroup

import Syntax

-- trace used variable names
type AllocId = String -> Maybe (Max Integer)

newId :: AllocId -> Id -> Id
newId f (Id x i) = Id x (maybe i (+1) (getMax <$> f x))

singleton :: Id -> AllocId
singleton (Id x i) y
  | x == y    = Just (Max i)
  | otherwise = Nothing

fromTerm :: Term -> AllocId
fromTerm (Var x) = singleton x
fromTerm (Lam _ _ e) = fromTerm e
fromTerm (App e1 e2) = fromTerm e1 <> fromTerm e2
fromTerm (Box _ _) = mempty
fromTerm (LetBox _ _ e1 e2) = fromTerm e1 <> fromTerm e2
fromTerm (Clo _ es) = mconcat (map fromTerm es)

-- substitutions
subst :: (Id, Term) -> Term -> Term
subst (x, e) (Var y)
  | y == x    = e
  | otherwise = Var y
subst s@(x, e) (Lam y t e1)
  | y == x    = Lam y t e1
  | otherwise =
    let y' = newId (singleton x <> fromTerm e) y
    in Lam y' t (subst s $ subst (y, Var y') $ e1)
subst s (App e1 e2) = App (subst s e1) (subst s e2)
subst s e@(Box _ _) = e
subst s (LetBox oectx u e1 e2) = LetBox oectx u (subst s e1) (subst s e2)
subst s (Clo u es) = Clo u (subst s <$> es)

substGlobal :: (GId, LECtx, Term) -> Term -> Term
substGlobal s e@(Var _) = e
substGlobal s (Lam x t e) = Lam x t (substGlobal s e)
substGlobal s (App e1 e2) = App (substGlobal s e1) (substGlobal s e2)
substGlobal s (Box octx oe) = Box octx (substGlobal s oe)
substGlobal s@(u, _, _) (LetBox oectx v e1 e2)
  | v == u    = LetBox oectx v (substGlobal s e1) e2
  | otherwise = LetBox oectx v (substGlobal s e1) (substGlobal s e2)
substGlobal s@(u,oectx,oe) e@(Clo v es)
  | v == u    = let es' = map (substGlobal s) es
                in substSim (Map.fromList $ zip oectx es') oe
  | otherwise = e

substSim :: Map Id Term -> Term -> Term
substSim s (Var x) = Map.findWithDefault (error "simultaneous substitution failed") x s
substSim s (Lam x t e) =
  let x' = newId (foldMap fromTerm s) x
      s' = Map.union (Map.singleton x (Var x')) s
  in Lam x' t (substSim s' e)
substSim s (App e1 e2) = App (substSim s e1) (substSim s e2)
substSim s e@(Box _ _) = e
substSim s (LetBox oectx u e1 e2) = LetBox oectx u (substSim s e1) (substSim s e2)
substSim s (Clo u es) = Clo u (substSim s <$> es)
