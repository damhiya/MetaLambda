module Reduction.Substitution where

import Data.Map (Map)
import qualified Data.Map as Map

import qualified Syntax.Object as O
import qualified Syntax.Meta as M

type Summary = Map String Integer

union :: Summary -> Summary -> Summary
union = Map.unionWith max

unions :: Foldable f => f Summary -> Summary
unions = Map.unionsWith max

-- meta language
singletonMeta :: M.Id -> Summary
singletonMeta (M.Id x i) = Map.singleton x i

sumMeta :: M.Tm -> Summary
sumMeta (M.Var x) = singletonMeta x
sumMeta (M.Abs _ _ tm) = sumMeta tm
sumMeta (M.App tm1 tm2) = union (sumMeta tm1) (sumMeta tm2)
sumMeta (M.Box _ _) = Map.empty
sumMeta (M.LetBox _ _ tm1 tm2) = union (sumMeta tm1) (sumMeta tm2)

newNameMeta :: Summary -> M.Id -> M.Id
newNameMeta s x@(M.Id name i) = maybe x (\i' -> M.Id name (i'+1)) (Map.lookup name s)

-- object language
singletonObject :: O.Id -> Summary
singletonObject (O.Id x i) = Map.singleton x i

sumObject :: O.Tm M.M -> Summary
sumObject (O.Var x) = singletonObject x
sumObject (O.Abs _ _ tm) = sumObject tm
sumObject (O.App tm1 tm2) = union (sumObject tm1) (sumObject tm2)
sumObject (O.Meta (M.Inst u t)) = unions (map sumObject t)

newNameObject :: Summary -> O.Id -> O.Id
newNameObject s x@(O.Id name i) = maybe x (\i' -> O.Id name (i'+1)) (Map.lookup name s)

-- substitutions
substMeta :: (M.Id, M.Tm) -> M.Tm -> M.Tm
substMeta (x, tm') tm@(M.Var y)
  | y == x    = tm'
  | otherwise = tm
substMeta s@(x, tm') tm@(M.Abs y ty tm1)
  | y == x    = tm
  | otherwise =
    let y' = newNameMeta (union (singletonMeta x) (sumMeta tm')) y
    in M.Abs y' ty (substMeta s $ substMeta (y, M.Var y') $ tm1)
substMeta s (M.App tm1 tm2) = M.App (substMeta s tm1) (substMeta s tm2)
substMeta s tm@(M.Box _ _) = tm
substMeta s (M.LetBox olectx u tm1 tm2) = M.LetBox olectx u (substMeta s tm1) (substMeta s tm2)

substGlobalMeta :: (M.GId, O.LECtx, O.Tm M.M) -> M.Tm -> M.Tm
substGlobalMeta s tm@(M.Var _) = tm
substGlobalMeta s (M.Abs x ty tm) = M.Abs x ty (substGlobalMeta s tm)
substGlobalMeta s (M.App tm1 tm2) = M.App (substGlobalMeta s tm1) (substGlobalMeta s tm2)
substGlobalMeta s (M.Box olctx otm) = M.Box olctx (substGlobalObject s otm)
substGlobalMeta s@(u, _, _) (M.LetBox olectx v tm1 tm2)
  | v == u    = M.LetBox olectx v (substGlobalMeta s tm1) tm2
  | otherwise = M.LetBox olectx v (substGlobalMeta s tm1) (substGlobalMeta s tm2)

substGlobalObject :: (M.GId, O.LECtx, O.Tm M.M) -> O.Tm M.M -> O.Tm M.M
substGlobalObject s tm@(O.Var _) = tm
substGlobalObject s (O.Abs x ty tm) = O.Abs x ty (substGlobalObject s tm)
substGlobalObject s (O.App tm1 tm2) = O.App (substGlobalObject s tm1) (substGlobalObject s tm2)
substGlobalObject s@(u,lectx,tm') tm@(O.Meta (M.Inst v t))
  | v == u = let t' = map (substGlobalObject s) t
             in substObjectSim (Map.fromList $ zip lectx t') tm'
                -- since tm' is a code template, it can't have instantiation in it.
  | otherwise = tm

substObjectSim :: Map O.Id (O.Tm M.M) -> O.Tm M.M -> O.Tm M.M
substObjectSim s tm@(O.Var x) = Map.findWithDefault (error "simultaneous substitution failed") x s
substObjectSim s (O.Abs x ty tm) =
  let x' = newNameObject (sumSubst s) x
      s' = Map.union (Map.singleton x (O.Var x')) s
  in O.Abs x' ty (substObjectSim s' tm)
  where
    sumSubst :: Map O.Id (O.Tm M.M) -> Summary
    sumSubst s = unions (Map.map sumObject s)
substObjectSim s (O.App tm1 tm2) = O.App (substObjectSim s tm1) (substObjectSim s tm2)
substObjectSim s tm@(O.Meta _) = error "object code has Meta"
