module MetaLambda.Substitution where

import           Data.Bifunctor
import           Data.Semigroup
import           Data.Set          (Set)
import qualified Data.Set          as Set

import           MetaLambda.Syntax

-- free variables
class FreeVar a where
  freeVar :: a -> Set Id

class FreeGVar a where
  freeGVar :: a -> Set GId

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
    TPrimOp op              -> go op
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
  freeVar = foldMap (freeVar . snd)

instance FreeGVar Term where
  freeGVar = \case
    TVar x                  -> mempty
    TTrue                   -> mempty
    TFalse                  -> mempty
    TBoolMatch e1 e2 e3     -> foldMap freeGVar [e1, e2, e3]
    TInt _                  -> mempty
    TPair e1 e2             -> foldMap freeGVar [e1, e2]
    TProdMatch e1 x y e2    -> foldMap freeGVar [e1, e2]
    TNil _                  -> mempty
    TCons e1 e2             -> foldMap freeGVar [e1, e2]
    TListMatch e1 e2 x y e3 -> foldMap freeGVar [e1, e2, e3]
    TLam x _ e              -> freeGVar e
    TFix _ _ f x e          -> freeGVar e
    TApp e1 e2              -> foldMap freeGVar [e1, e2]
    TBox octx oe            -> freeGVar oe
    TLetBox u e1 e2         -> mconcat [ freeGVar e1
                                       , freeGVar e2 `Set.difference` Set.singleton u
                                       ]
    TClo u s                -> mconcat [ Set.singleton u
                                       , freeGVar s
                                       ]
    TLet x e1 e2            -> foldMap freeGVar [e1, e2]
    TPrimOp op              -> go op
    where
      go (IntEq  e1 e2) = freeGVar e1 <> freeGVar e2
      go (IntLe  e1 e2) = freeGVar e1 <> freeGVar e2
      go (IntLt  e1 e2) = freeGVar e1 <> freeGVar e2
      go (IntAdd e1 e2) = freeGVar e1 <> freeGVar e2
      go (IntSub e1 e2) = freeGVar e1 <> freeGVar e2
      go (IntMul e1 e2) = freeGVar e1 <> freeGVar e2
      go (IntDiv e1 e2) = freeGVar e1 <> freeGVar e2
      go (IntMod e1 e2) = freeGVar e1 <> freeGVar e2
      go (IntPow e1 e2) = freeGVar e1 <> freeGVar e2
      go (Inject e)     = freeGVar e

instance FreeGVar Subst where
  freeGVar = foldMap (freeGVar . snd)

freshId :: Set Id -> Id -> Id
freshId fv (Id x i) = Id x i'
  where
    go :: Id -> Maybe (Max Integer)
    go (Id y j) | y == x    = Just (Max j)
                | otherwise = Nothing
    a  = getMax <$> foldMap go fv
    i' = maybe i (+1) a

freshGId :: Set GId -> GId -> GId
freshGId fv (GId x i) = GId x i'
  where
    go :: GId -> Maybe (Max Integer)
    go (GId y j) | y == x    = Just (Max j)
                 | otherwise = Nothing
    a = getMax <$> foldMap go fv
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

renameGVar :: (GId, GId) -> Term -> Term
renameGVar = go
  where
    go r@(u,v) = \case
      TVar x                      -> TVar x
      TTrue                       -> TTrue
      TFalse                      -> TFalse
      TBoolMatch e1 e2 e3         -> TBoolMatch (go r e1) (go r e2) (go r e3)
      TInt n                      -> TInt n
      TPair e1 e2                 -> TPair (go r e1) (go r e2)
      TProdMatch e1 x y e2        -> TProdMatch (go r e1) x y (go r e2)
      TNil t                      -> TNil t
      TCons e1 e2                 -> TCons (go r e1) (go r e2)
      TListMatch e1 e2 x y e3     -> TListMatch (go r e1) (go r e2) x y (go r e3)
      TLam x t e                  -> TLam x t (go r e)
      TFix t1 t2 f x e            -> TFix t1 t2 f x (go r e)
      TApp e1 e2                  -> TApp (go r e1) (go r e2)
      TBox octx oe                -> TBox octx (go r oe)
      TLetBox w e1 e2 | w == u    -> TLetBox w (go r e1) e2
                      | otherwise -> TLetBox w (go r e1) (go r e2)
      TClo w s | w == u           -> TClo v (renameGVar' r s)
               | otherwise        -> TClo w (renameGVar' r s)
      TLet x e1 e2                -> TLet x (go r e1) (go r e2)
      TPrimOp op                  -> TPrimOp (primop op)
      where
        primop (IntEq  e1 e2) = IntEq  (go r e1) (go r e2)
        primop (IntLe  e1 e2) = IntLe  (go r e1) (go r e2)
        primop (IntLt  e1 e2) = IntLt  (go r e1) (go r e2)
        primop (IntAdd e1 e2) = IntAdd (go r e1) (go r e2)
        primop (IntSub e1 e2) = IntSub (go r e1) (go r e2)
        primop (IntMul e1 e2) = IntMul (go r e1) (go r e2)
        primop (IntDiv e1 e2) = IntDiv (go r e1) (go r e2)
        primop (IntMod e1 e2) = IntMod (go r e1) (go r e2)
        primop (IntPow e1 e2) = IntPow (go r e1) (go r e2)
        primop (Inject e)     = Inject (go r e)

renameGVar' :: (GId, GId) -> Subst -> Subst
renameGVar' r s = second (renameGVar r) <$> s

applyGSubst :: (GId, (LECtx, Term)) -> Term -> Term
applyGSubst = go
  where
    go gs@(u,(_,eu)) = \case
      TVar x                  -> TVar x
      TTrue                   -> TTrue
      TFalse                  -> TFalse
      TBoolMatch e1 e2 e3     -> TBoolMatch (go gs e1) (go gs e2) (go gs e3)
      TInt n                  -> TInt n
      TPair e1 e2             -> TPair (go gs e1) (go gs e2)
      TProdMatch e1 x y e2    -> TProdMatch (go gs e1) x y (go gs e2)
      TNil t                  -> TNil t
      TCons e1 e2             -> TCons (go gs e1) (go gs e2)
      TListMatch e1 e2 x y e3 -> TListMatch (go gs e1) (go gs e2) x y (go gs e3)
      TLam x t e              -> TLam x t (go gs e)
      TFix t1 t2 f x e        -> TFix t1 t2 f x (go gs e)
      TApp e1 e2              -> TApp (go gs e1) (go gs e2)
      TBox octx oe            -> TBox octx (go gs oe)
      TLetBox v e1 e2         -> TLetBox v' (go gs e1) (go gs e2')
        where
          fv = freeGVar eu <> Set.singleton u
          v' = freshGId fv v
          e2' = renameGVar (v, v') e2
      TClo v s | v == u       -> applySubst s' eu
               | otherwise    -> TClo v s'
        where
          s' = applyGSubst' gs s
      TLet x e1 e2            -> TLet x (go gs e1) (go gs e2)
      TPrimOp op              -> TPrimOp (primop op)
      where
        primop (IntEq  e1 e2) = IntEq  (go gs e1) (go gs e2)
        primop (IntLe  e1 e2) = IntLe  (go gs e1) (go gs e2)
        primop (IntLt  e1 e2) = IntLt  (go gs e1) (go gs e2)
        primop (IntAdd e1 e2) = IntAdd (go gs e1) (go gs e2)
        primop (IntSub e1 e2) = IntSub (go gs e1) (go gs e2)
        primop (IntMul e1 e2) = IntMul (go gs e1) (go gs e2)
        primop (IntDiv e1 e2) = IntDiv (go gs e1) (go gs e2)
        primop (IntMod e1 e2) = IntMod (go gs e1) (go gs e2)
        primop (IntPow e1 e2) = IntPow (go gs e1) (go gs e2)
        primop (Inject e)     = Inject (go gs e)

applyGSubst' :: (GId, (LECtx, Term)) -> Subst -> Subst
applyGSubst' gs s = second (applyGSubst gs) <$> s
