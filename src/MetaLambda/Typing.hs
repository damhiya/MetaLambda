module MetaLambda.Typing where

import           Numeric.Natural
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

-- Context and it's operations
type Ctxs = [Ctx]

appendCtx :: Mode -> Ctxs -> Mode -> (Id, Type) -> Ctxs
appendCtx = \m ctxs n xa -> go ctxs (n-m) xa
  where
    go []         0 xa = [xa]     : []
    go (ctx:ctxs) 0 xa = (xa:ctx) : ctxs
    go []         n xa = []       : go []   (n-1) xa
    go (ctx:ctxs) n xa = ctx      : go ctxs (n-1) xa

infixl 5 @>
(@>) :: Ctx -> (Id, Type) -> Ctx
(@>) = flip (:)

localCtx :: Ctxs -> Ctx
localCtx []         = []
localCtx (ctx:ctxs) = ctx

globalCtx :: Mode -> Ctxs -> Mode -> Ctxs
globalCtx = \m ctxs n -> go ctxs (n-m)
  where
    go ctxs     0 = ctxs
    go []       n = []
    go (_:ctxs) n = go ctxs (n-1)

lookupId :: Ctx -> Id -> Maybe Type
lookupId ctx x = lookup x ctx

-- type inference
inferType :: MonadError TypeError m => Mode -> Ctxs -> Term -> m Type
inferType m ctxs (Var x) = with LookUpError $ lookupId (localCtx ctxs) x
inferType m ctxs (Lam x a t) = do
  b <- inferType m (appendCtx m ctxs m (x,a)) t
  pure (Arr a b)
inferType m ctxs (App t1 t2) = do
  inferType m ctxs t1 >>= \case
    Arr a b -> do
      a' <- inferType m ctxs t2
      guardWith GuardError (a == a')
      pure b
    _ -> throwError MatchError
inferType m ctxs (Lift ctx t) = do
  guardWith GuardError (m > 0)
  a <- inferType (m - 1) (ctx:ctxs) t
  pure (Upshift ctx a)
inferType m ctxs (Unlift t s) = do
  inferType (m + 1) (globalCtx m ctxs (m+1)) t >>= \case
    Upshift ctx a -> do
      forM_ (zip (map snd ctx) s) $ \(a, t) -> do
        a' <- inferType m ctxs t
        guardWith GuardError (a == a')
      pure a
    _ -> throwError MatchError
inferType m ctxs (Return t) = do
  a <- inferType (m + 1) (globalCtx m ctxs (m+1)) t
  pure (Downshift a)
inferType m ctxs (LetReturn u t1 t2) = do
  inferType m ctxs t1 >>= \case
    Downshift a -> do
      b <- inferType m (appendCtx m ctxs (m+1) (u,a)) t2
      pure b
    _ -> throwError MatchError
