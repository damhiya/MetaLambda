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

appendCtx :: Ctxs -> Natural -> (Id, Type) -> Ctxs
appendCtx []         0 xa = [[xa]]
appendCtx (ctx:ctxs) 0 xa = (xa:ctx) : ctxs
appendCtx []         n xa = []       : appendCtx [] (n-1) xa
appendCtx (ctx:ctxs) n xa = ctx      : appendCtx ctxs (n-1) xa

infixl 5 @>
(@>) :: Ctx -> (Id, Type) -> Ctx
(@>) = flip (:)

localCtx :: Ctxs -> Ctx
localCtx []         = []
localCtx (ctx:ctxs) = ctx

globalCtx :: Ctxs -> Ctxs
globalCtx []         = []
globalCtx (ctx:ctxs) = ctxs

lookupId :: Ctx -> Id -> Maybe Type
lookupId ctx x = lookup x ctx

-- type inference
inferType :: MonadError TypeError m => Mode -> Ctxs -> Term -> m Type
inferType m ctxs (Var x) = with LookUpError $ lookupId (localCtx ctxs) x
inferType m ctxs (Lam x a t) = do
  b <- inferType m (appendCtx ctxs 0 (x,a)) t
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
  inferType (m + 1) (globalCtx ctxs) t >>= \case
    Upshift ctx a -> do
      forM_ (zip (map snd ctx) s) $ \(a, t) -> do
        a' <- inferType m ctxs t
        guardWith GuardError (a == a')
      pure a
    _ -> throwError MatchError
inferType m ctxs (Return t) = do
  a <- inferType (m + 1) (globalCtx ctxs) t
  pure (Downshift a)
inferType m ctxs (LetReturn u t1 t2) = do
  inferType m ctxs t1 >>= \case
    Downshift a -> do
      b <- inferType m (appendCtx ctxs 1 (u,a)) t2
      pure b
    _ -> throwError MatchError
