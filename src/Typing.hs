{-# LANGUAGE LambdaCase #-}

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

data Error
  = LookUpError
  | MatchError
  | GuardError
  deriving Show

with :: e -> Maybe a -> Either e a
with e Nothing = Left e
with e (Just x) = Right x

inferType :: GCtx -> LCtx -> Term -> Either Error Type
inferType gctx ctx (Var x) = with LookUpError $ lookupId ctx x
inferType gctx ctx (Lam x ta e) = do
  tb <- inferType gctx ((x,ta) : ctx) e
  pure (Arr ta tb)
inferType gctx ctx (App e1 e2) = do
  inferType gctx ctx e1 >>= \case
    Arr ta tb -> do
      ta' <- inferType gctx ctx e2
      with GuardError $ guard (ta == ta')
      pure tb
    _ -> Left MatchError
inferType gctx ctx (Box octx oe) = do
  ot <- inferType gctx octx oe
  pure (BoxT octx ot)
inferType gctx ctx (LetBox oectx u e1 e2) =
  inferType gctx ctx e1 >>= \case
    BoxT octx ot -> do
      with GuardError $ guard (oectx == erase octx)
      inferType ((u, octx, ot) : gctx) ctx e2
    _ -> Left MatchError
inferType gctx ctx (Clo u es) = do
  (octx, ot) <- with LookUpError $ lookupGId gctx u
  with GuardError $ guard (length octx == length es)
  let ts = map snd octx
  forM_ (zip ts es) $ \(t, e) -> do
    t' <- inferType gctx ctx e
    with GuardError $ guard (t == t')
  pure ot
