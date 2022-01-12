module Typing where

import Control.Monad

import Syntax

-- global context
type GCtx = [(GId, LCtx, Type)]

-- lookup functions
lookupId :: LCtx -> Id -> Maybe Type
lookupId ctx x = lookup x ctx

lookupGId :: GCtx -> GId -> Maybe (LCtx, Type)
lookupGId gctx x = lookup x $ map (\(x,y,z) -> (x,(y,z))) gctx

-- erase type in the context
erase :: LCtx -> LECtx
erase = map fst

-- type inference
-- x for id, t for type, e for expression
inferType :: GCtx -> LCtx -> Term -> Maybe Type
inferType gctx ctx (Var x) = lookupId ctx x
inferType gctx ctx (Lam x ta e) = do
  tb <- inferType gctx ((x,ta) : ctx) e
  pure (Arr ta tb)
inferType gctx ctx (App e1 e2) = do
  Arr ta tb <- inferType gctx ctx e1
  ta' <- inferType gctx ctx e2
  guard (ta == ta')
  pure tb
inferType gctx ctx (Box octx oe) = do
  ot <- inferType gctx octx oe
  pure (BoxT octx ot)
inferType gctx ctx (LetBox oectx u e1 e2) = do
  BoxT octx ot <- inferType gctx ctx e1
  guard (oectx == erase octx)
  inferType ((u, octx, ot) : gctx) ctx e2
inferType gctx ctx (Clo u es) = do
  (octx, ot) <- lookupGId gctx u
  guard (length octx == length es)
  let ts = map snd ctx
  forM_ (zip ts es) $ \(t, e) -> do
    t' <- inferType gctx ctx e
    guard (t == t')
  pure ot
