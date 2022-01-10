module Typing where

import Control.Monad

import qualified Syntax.Object as O
import qualified Syntax.Meta as M

-- global context
type GCtx = [(M.GId, O.LCtx, O.Ty)]

-- lookup functions
lookupObjId :: O.LCtx -> O.Id -> Maybe O.Ty
lookupObjId ctx x = lookup x ctx

lookupMetaId :: M.LCtx -> M.Id -> Maybe M.Ty
lookupMetaId ctx x = lookup x ctx

lookupGId :: GCtx -> M.GId -> Maybe (O.LCtx, O.Ty)
lookupGId ctx x = lookup x $ map (\(x,y,z) -> (x,(y,z))) ctx

-- type inference
inferMeta :: GCtx -> M.LCtx -> M.Tm -> Maybe M.Ty
inferMeta gctx lctx (M.Var x) = lookupMetaId lctx x
inferMeta gctx lctx (M.Abs x ty tm) = do
  ty' <- inferMeta gctx ((x,ty) : lctx) tm
  pure (M.Arr ty ty')
inferMeta gctx lctx (M.App tm1 tm2) = do
  M.Arr tya tyb <- inferMeta gctx lctx tm1
  tya' <- inferMeta gctx lctx tm2
  guard (tya == tya')
  pure tyb
inferMeta gctx lctx (M.Box olctx otm) = do
  oty <- inferObj gctx olctx otm
  pure (M.BoxT olctx oty)
inferMeta gctx lctx (M.LetBox olectx x tm1 tm2) = do
  M.BoxT olctx oty <- inferMeta gctx lctx tm1
  guard (length olectx == length olctx)
  inferMeta ((x, olctx, oty) : gctx) lctx tm2

inferObj :: GCtx -> O.LCtx -> O.Tm M.M -> Maybe O.Ty
inferObj gctx lctx (O.Var x) = lookupObjId lctx x
inferObj gctx lctx (O.Abs x ty tm) = do
  ty' <- inferObj gctx ((x, ty) : lctx) tm
  pure (O.Arr ty ty')
inferObj gctx lctx (O.App tm1 tm2) = do
  O.Arr tya tyb <- inferObj gctx lctx tm1
  tya' <- inferObj gctx lctx tm2
  guard (tya == tya')
  pure tyb
inferObj gctx lctx (O.Meta (M.Inst x tms)) = do
  (ctx, ty) <- lookupGId gctx x
  guard (length ctx == length tms)
  let tys = map snd ctx
  forM_ (zip tys tms) $ \(ty, tm) -> do
    ty' <- inferObj gctx lctx tm
    guard (ty == ty')
  pure ty

inferType :: M.Tm -> Maybe M.Ty
inferType = inferMeta [] []
