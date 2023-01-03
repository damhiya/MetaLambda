module MetaLambda.Typing where

import           Control.Monad
import           Control.Monad.Except
import qualified Data.Map             as M
import qualified Data.MapList         as ML

import           MetaLambda.Syntax
import           Util

-- error
data TypeError
  = LookUpError
  | MatchError
  | GuardError
  deriving Show

-- Context operations
appendVar :: Ord mo => Ctxs mo -> mo -> (Id, Type mo) -> Ctxs mo
appendVar ctxs m xa = ML.consAt m xa ctxs

appendCtx :: Ord mo => Ctxs mo -> mo -> Ctx mo -> Ctxs mo
appendCtx ctxs m ctx = ML.insert m ctx ctxs

infixl 5 @>
(@>) :: Ctx mo -> (Id, Type mo) -> Ctx mo
(@>) = flip (:)

localCtx :: Ord mo => Ctxs mo -> mo -> Ctx mo
localCtx ctxs m = ML.lookup m ctxs

globalCtx :: (Mode mo, Ord mo) => Ctxs mo -> mo -> Ctxs mo
globalCtx ctxs m = M.filterWithKey (\n _ -> globalCtxOf m n) ctxs

lookupId :: Ctx mo -> Id -> Maybe (Type mo)
lookupId ctx x = lookup x ctx

-- type inference
inferType :: (Mode mo, Ord mo, MonadError TypeError m) => mo -> Ctxs mo -> Term mo -> m (Type mo)
inferType m ctxs (Var x) = with LookUpError $ lookupId (localCtx ctxs m) x
inferType m ctxs (Lam x a t) = do
  b <- inferType m (appendVar ctxs m (x,a)) t
  pure (Arr a b)
inferType m ctxs (App t1 t2) = do
  inferType m ctxs t1 >>= \case
    Arr a b -> do
      a' <- inferType m ctxs t2
      guardWith GuardError (a == a')
      pure b
    _ -> throwError MatchError
inferType m ctxs (Lift n ctx t) = do
  guardWith GuardError (hasUpshift n m)
  a <- inferType n (appendCtx ctxs n ctx) t
  pure (Upshift n ctx a)
inferType m ctxs (Unlift n t s) = do
  guardWith GuardError (hasUpshift m n)
  inferType n (globalCtx ctxs n) t >>= \case
    Upshift m' ctx a -> do
      guardWith GuardError (m == m')
      forM_ (zip (map snd ctx) s) $ \(a, t) -> do
        a' <- inferType m ctxs t
        guardWith GuardError (a == a')
      pure a
    _ -> throwError MatchError
inferType m ctxs (Return n t) = do
  guardWith GuardError (hasDownshift m n)
  a <- inferType n (globalCtx ctxs n) t
  pure (Downshift n a)
inferType m ctxs (LetReturn n u t1 t2) = do
  guardWith GuardError (hasDownshift m n)
  inferType m ctxs t1 >>= \case
    Downshift n' a -> do
      guardWith GuardError (n == n')
      b <- inferType m (appendVar ctxs n (u,a)) t2
      pure b
    _ -> throwError MatchError
