module MetaLambda.Typing where

import           Control.Monad
import           Control.Monad.Except

import           MetaLambda.Syntax
import           Util

-- error
data TypeError
  = LookUpError
  | MatchError
  | GuardError
  deriving Show

-- type checking
inferType :: MonadError TypeError m => GCtx -> LCtx -> Term -> m Type
inferType gctx ctx (TVar x) = with LookUpError $ lookupId ctx x
inferType gctx ctx TTrue = pure Bool
inferType gctx ctx TFalse = pure Bool
inferType gctx ctx (TBoolMatch e0 e1 e2) = do
  inferType gctx ctx e0 >>= \case
    Bool -> do
      t1 <- inferType gctx ctx e1
      t2 <- inferType gctx ctx e2
      with GuardError $ guard (t1 == t2)
      pure t1
    _ -> throwError MatchError
inferType gctx ctx (TInt n) = pure Int
inferType gctx ctx (TPair e1 e2) = do
  t1 <- inferType gctx ctx e1
  t2 <- inferType gctx ctx e2
  pure (Prod t1 t2)
inferType gctx ctx (TProdMatch e1 x y e2) = do
  inferType gctx ctx e1 >>= \case
    Prod tx ty -> do
      t <- inferType gctx ((x,tx) : (y,ty) : ctx) e2
      pure t
    _ -> throwError MatchError
inferType gctx ctx (TNil t) = do
  pure (List t)
inferType gctx ctx (TCons e1 e2) = do
  t1 <- inferType gctx ctx e1
  t2 <- inferType gctx ctx e2
  case t2 of
    List t -> do
      with GuardError $ guard (t1 == t)
      pure t2
    _ -> throwError MatchError
inferType gctx ctx (TListMatch e enil x xs econs) = do
  inferType gctx ctx e >>= \case
    List t -> do
      t1 <- inferType gctx ctx enil
      t2 <- inferType gctx ((x,t) : (xs,List t) : ctx) econs
      with GuardError $ guard (t1 == t2)
      pure t1
    _ -> throwError MatchError
inferType gctx ctx (TLam x ta e) = do
  tb <- inferType gctx ((x,ta) : ctx) e
  pure (Arr ta tb)
inferType gctx ctx (TFix t1 t2 f x e) = do
  t2' <- inferType gctx ((f,Arr t1 t2) : (x,t1) : ctx) e
  with GuardError $ guard (t2 == t2')
  pure (Arr t1 t2)
inferType gctx ctx (TApp e1 e2) = do
  inferType gctx ctx e1 >>= \case
    Arr ta tb -> do
      ta' <- inferType gctx ctx e2
      with GuardError $ guard (ta == ta')
      pure tb
    _ -> throwError MatchError
inferType gctx ctx (TBox octx oe) = do
  ot <- inferType gctx octx oe
  pure (Box octx ot)
inferType gctx ctx (TLetBox u e1 e2) =
  inferType gctx ctx e1 >>= \case
    Box octx ot -> do
      inferType ((u, (octx, ot)) : gctx) ctx e2
    _ -> throwError MatchError
inferType gctx ctx (TClo u s) = do
  (octx, ot) <- with LookUpError $ lookupGId gctx u
  octx' <- inferTypeSubst gctx ctx s
  with GuardError $ guard (octx == octx')
  pure ot
inferType gctx ctx (TLet x e1 e2) = do
  t <- inferType gctx ctx e1
  inferType gctx ((x,t) : ctx) e2
inferType gctx ctx (TPrimOp op) = go op
  where
    go (IntEq e1 e2)  = comparator e1 e2
    go (IntLe e1 e2)  = comparator e1 e2
    go (IntLt e1 e2)  = comparator e1 e2
    go (IntAdd e1 e2) = binoperator e1 e2
    go (IntSub e1 e2) = binoperator e1 e2
    go (IntMul e1 e2) = binoperator e1 e2
    go (IntDiv e1 e2) = binoperator e1 e2
    go (IntMod e1 e2) = binoperator e1 e2
    go (IntPow e1 e2) = binoperator e1 e2
    go (Inject e)     = inject e
    comparator e1 e2 = do
      t1 <- inferType gctx ctx e1
      t2 <- inferType gctx ctx e2
      with GuardError $ guard (t1 == Int && t2 == Int)
      pure Bool
    binoperator e1 e2 = do
      t1 <- inferType gctx ctx e1
      t2 <- inferType gctx ctx e2
      with GuardError $ guard (t1 == Int && t2 == Int)
      pure Int
    inject e = do
      inferType gctx ctx e >>= \case
        Int -> pure (Box [] Int)
        _ -> throwError MatchError

inferTypeSubst :: MonadError TypeError m => GCtx -> LCtx -> Subst -> m LCtx
inferTypeSubst gctx ctx [] = pure []
inferTypeSubst gctx ctx ((x,e) : s) = do
  ctx' <- inferTypeSubst gctx ctx s
  t <- inferType gctx ctx e
  pure ((x,t) : ctx')
