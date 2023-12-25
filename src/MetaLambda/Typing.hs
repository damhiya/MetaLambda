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

-- global context
type GCtx = [(GId, LCtx, Type)]

-- lookup functions
lookupId :: LCtx -> Id -> Maybe Type
lookupId ctx x = lookup x ctx

lookupGId :: GCtx -> GId -> Maybe (LCtx, Type)
lookupGId gctx u = lookup u $ map (\(x,y,z) -> (x,(y,z))) gctx

-- erase type in the context
erase :: LCtx -> LECtx
erase = map fst

inferType :: MonadError TypeError m => GCtx -> LCtx -> Term -> m Type
inferType gctx ctx (Var x) = with LookUpError $ lookupId ctx x
inferType gctx ctx BTrue = pure Bool
inferType gctx ctx BFalse = pure Bool
inferType gctx ctx (BoolMatch e0 e1 e2) = do
  inferType gctx ctx e0 >>= \case
    Bool -> do
      t1 <- inferType gctx ctx e1
      t2 <- inferType gctx ctx e2
      with GuardError $ guard (t1 == t2)
      pure t1
    _ -> throwError MatchError
inferType gctx ctx (IntLit n) = pure Int
inferType gctx ctx (Pair e1 e2) = do
  t1 <- inferType gctx ctx e1
  t2 <- inferType gctx ctx e2
  pure (Prod t1 t2)
inferType gctx ctx (ProdMatch e1 x y e2) = do
  inferType gctx ctx e1 >>= \case
    Prod tx ty -> do
      t <- inferType gctx ((x,tx) : (y,ty) : ctx) e2
      pure t
    _ -> throwError MatchError
inferType gctx ctx (Nil t) = do
  pure (List t)
inferType gctx ctx (Cons e1 e2) = do
  t1 <- inferType gctx ctx e1
  t2 <- inferType gctx ctx e2
  case t2 of
    List t -> do
      with GuardError $ guard (t1 == t)
      pure t2
    _ -> throwError MatchError
inferType gctx ctx (ListMatch e enil x xs econs) = do
  inferType gctx ctx e >>= \case
    List t -> do
      t1 <- inferType gctx ctx enil
      t2 <- inferType gctx ((x,t) : (xs,List t) : ctx) econs
      with GuardError $ guard (t1 == t2)
      pure t1
    _ -> throwError MatchError
inferType gctx ctx (Lam x ta e) = do
  tb <- inferType gctx ((x,ta) : ctx) e
  pure (Arr ta tb)
inferType gctx ctx (Fix t1 t2 f x e) = do
  t2' <- inferType gctx ((f,Arr t1 t2) : (x,t1) : ctx) e
  with GuardError $ guard (t2 == t2')
  pure (Arr t1 t2)
inferType gctx ctx (App e1 e2) = do
  inferType gctx ctx e1 >>= \case
    Arr ta tb -> do
      ta' <- inferType gctx ctx e2
      with GuardError $ guard (ta == ta')
      pure tb
    _ -> throwError MatchError
inferType gctx ctx (Box octx oe) = do
  ot <- inferType gctx octx oe
  pure (BoxT octx ot)
inferType gctx ctx (LetBox oectx u e1 e2) =
  inferType gctx ctx e1 >>= \case
    BoxT octx ot -> do
      with GuardError $ guard (oectx == erase octx)
      inferType ((u, octx, ot) : gctx) ctx e2
    _ -> throwError MatchError
inferType gctx ctx (Clo u es) = do
  (octx, ot) <- with LookUpError $ lookupGId gctx u
  with GuardError $ guard (length octx == length es)
  let ts = map snd octx
  forM_ (zip ts es) $ \(t, e) -> do
    t' <- inferType gctx ctx e
    with GuardError $ guard (t == t')
  pure ot
inferType gctx ctx (Let x e1 e2) = do
  t <- inferType gctx ctx e1
  inferType gctx ((x,t) : ctx) e2
inferType gctx ctx (PrimOp op) = go op
  where
    go (IntEq e1 e2)  = comparator e1 e2
    go (IntLe e1 e2)  = comparator e1 e2
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
        Int -> pure (BoxT [] Int)
        _ -> throwError MatchError
