module Reduction.Substitution where

import Data.Map (Map)
import qualified Data.Map as Map

import qualified Syntax.Object as O
import qualified Syntax.Meta as M

type Summary = Map String Integer

union :: Summary -> Summary -> Summary
union = Map.unionWith max

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

substObject :: (O.Id, O.Tm M.M) -> O.Tm M.M -> O.Tm M.M
substObject s tm = undefined

substGlobal :: (M.GId, O.LECtx, O.Tm M.M) -> M.Tm -> M.Tm
substGlobal s tm = undefined

substObjectSim :: [(O.Id, O.Tm M.M)] -> O.Tm M.M -> O.Tm M.M
substObjectSim s tm = undefined
