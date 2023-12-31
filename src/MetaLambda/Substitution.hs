module MetaLambda.Substitution where

import           Data.Bifunctor
import           Data.Semigroup
import           Data.Set          (Set)
import qualified Data.Set          as Set

import           MetaLambda.Syntax

-- free variables
class FreeVar a where
  freeVar :: a -> Set Id

instance FreeVar Term where
  freeVar = \case
    TVar x                  -> Set.singleton x
    TTrue                   -> mempty
    TFalse                  -> mempty
    TBoolMatch e1 e2 e3     -> freeVar e1 <> freeVar e2 <> freeVar e3
    TInt _                  -> mempty
    TPair e1 e2             -> freeVar e1 <> freeVar e2
    TProdMatch e1 x y e2    -> mconcat [ freeVar e1
                                       , freeVar e2 `Set.difference` Set.fromList [x,y]
                                       ]
    TNil _                  -> mempty
    TCons e1 e2             -> freeVar e1 <> freeVar e2
    TListMatch e1 e2 x y e3 -> mconcat [ freeVar e1
                                       , freeVar e2
                                       , freeVar e3 `Set.difference` Set.fromList [x,y]
                                       ]
    TLam x _ e              -> freeVar e `Set.difference` Set.singleton x
    TFix _ _ f x e          -> freeVar e `Set.difference` Set.fromList [f,x]
    TApp e1 e2              -> freeVar e1 <> freeVar e2
    TBox _ _                -> mempty
    TLetBox _ e1 e2         -> freeVar e1 <> freeVar e2
    TClo _ s                -> freeVar s
    TLet x e1 e2            -> mconcat [ freeVar e1
                                       , freeVar e2 `Set.difference` Set.singleton x
                                       ]
    TPrimOp op -> go op
    where
      go (IntEq  e1 e2) = freeVar e1 <> freeVar e2
      go (IntLe  e1 e2) = freeVar e1 <> freeVar e2
      go (IntLt  e1 e2) = freeVar e1 <> freeVar e2
      go (IntAdd e1 e2) = freeVar e1 <> freeVar e2
      go (IntSub e1 e2) = freeVar e1 <> freeVar e2
      go (IntMul e1 e2) = freeVar e1 <> freeVar e2
      go (IntDiv e1 e2) = freeVar e1 <> freeVar e2
      go (IntMod e1 e2) = freeVar e1 <> freeVar e2
      go (IntPow e1 e2) = freeVar e1 <> freeVar e2
      go (Inject e)     = freeVar e

instance FreeVar Subst where
  freeVar = mconcat . map (freeVar . snd)

freshId :: Set Id -> Id -> Id
freshId fv (Id x i) = Id x i'
  where
    go :: Id -> Maybe (Max Integer)
    go (Id y j) | y == x    = Just (Max j)
                | otherwise = Nothing
    a  = getMax <$> foldMap go fv
    i' = maybe i (+1) a

-- substitutions
applySubst :: Subst -> Term -> Term
applySubst = go
  where
    go s = \case
      TVar x                  -> case lookupSubst s x of
                                   Just e  -> e
                                   Nothing -> error "applySubst failed"
      TTrue                   -> TTrue
      TFalse                  -> TFalse
      TBoolMatch e1 e2 e3     -> TBoolMatch (go s e1) (go s e2) (go s e3)
      TInt n                  -> TInt n
      TPair e1 e2             -> TPair (go s e1) (go s e2)
      TProdMatch e1 x y e2    -> TProdMatch (go s e1) x' y' (go s' e2)
        where
          x' = freshId fv x
          y' = freshId (fv <> Set.singleton x') y
          s' = (y, TVar y') : (x, TVar x') : s
      TNil t                  -> TNil t
      TCons e1 e2             -> TCons (go s e1) (go s e2)
      TListMatch e1 e2 x y e3 -> TListMatch (go s e1) (go s e2) x' y' (go s' e3)
        where
          x' = freshId fv x
          y' = freshId (fv <> Set.singleton x') y
          s' = (y, TVar y') : (x, TVar x') : s
      TLam x t e              -> TLam x' t (go s' e)
        where
          x' = freshId fv x
          s' = (x, TVar x') : s
      TFix t1 t2 f x e        -> TFix t1 t2 f' x' (go s' e)
        where
          f' = freshId fv f
          x' = freshId (fv <> Set.singleton f') x
          s' = (x, TVar x') : (f, TVar f') : s
      TApp e1 e2             -> TApp (go s e1) (go s e2)
      TBox octx e            -> TBox octx e
      TLetBox u e1 e2        -> TLetBox u (go s e1) (go s e2)
      TClo u s'              -> TClo u (composeSubst s s')
      TLet x e1 e2           -> TLet x' (go s e1) (go s' e2)
        where
          x' = freshId fv x
          s' = (x, TVar x') : s
      TPrimOp op -> TPrimOp (primop op)
      where
        fv = freeVar s
        primop (IntEq  e1 e2) = IntEq  (go s e1) (go s e2)
        primop (IntLe  e1 e2) = IntLe  (go s e1) (go s e2)
        primop (IntLt  e1 e2) = IntLt  (go s e1) (go s e2)
        primop (IntAdd e1 e2) = IntAdd (go s e1) (go s e2)
        primop (IntSub e1 e2) = IntSub (go s e1) (go s e2)
        primop (IntMul e1 e2) = IntMul (go s e1) (go s e2)
        primop (IntDiv e1 e2) = IntDiv (go s e1) (go s e2)
        primop (IntMod e1 e2) = IntMod (go s e1) (go s e2)
        primop (IntPow e1 e2) = IntPow (go s e1) (go s e2)
        primop (Inject e)     = Inject (go s e)

-- applySubst (composeSubst s1 s2) t = applySubst s1 (applySubst s2 t)
composeSubst :: Subst -> Subst -> Subst
composeSubst s1 s2 = second (applySubst s1) <$> s2

substGlobal :: (GId, LECtx, Term) -> Term -> Term
substGlobal s e@(TVar _) = e
substGlobal s TTrue = TTrue
substGlobal s TFalse = TFalse
substGlobal s (TBoolMatch e0 e1 e2) = TBoolMatch (substGlobal s e0) (substGlobal s e1) (substGlobal s e2)
substGlobal s (TInt n) = TInt n
substGlobal s (TPair e1 e2) = TPair (substGlobal s e1) (substGlobal s e2)
substGlobal s (TProdMatch e0 x y e1) = TProdMatch (substGlobal s e0) x y (substGlobal s e1)
substGlobal s (TNil t) = TNil t
substGlobal s (TCons e0 e1) = TCons (substGlobal s e0) (substGlobal s e1)
substGlobal s (TListMatch e0 e1 x xs e2) = TListMatch (substGlobal s e0) (substGlobal s e1) x xs (substGlobal s e2)
substGlobal s (TLam x t e) = TLam x t (substGlobal s e)
substGlobal s (TFix t1 t2 f x e) = TFix t1 t2 f x (substGlobal s e)
substGlobal s (TApp e1 e2) = TApp (substGlobal s e1) (substGlobal s e2)
substGlobal s (TBox octx oe) = TBox octx (substGlobal s oe)
substGlobal s@(u, _, _) (TLetBox v e1 e2)
  | v == u    = TLetBox v (substGlobal s e1) e2
  | otherwise = TLetBox v (substGlobal s e1) (substGlobal s e2) -- FIXME : rename v
substGlobal s@(u,oectx,oe) e@(TClo v es)
  | v == u    = let es' = second (substGlobal s) <$> es
                in applySubst es' oe
  | otherwise = e -- FIXME : apply s to es
substGlobal s (TLet x e1 e2) = TLet x (substGlobal s e1) (substGlobal s e2)
substGlobal s (TPrimOp op) = TPrimOp (substGlobal s <$> op)
