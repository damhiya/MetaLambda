module MetaLambda.Substitution where

import           Data.Bifunctor
import           Data.Map          (Map)
import qualified Data.Map          as Map
import           Data.Semigroup

import           MetaLambda.Syntax

-- trace used variable names
type AllocId = String -> Maybe (Max Integer)

newId :: AllocId -> Id -> Id
newId f (Id x i) = Id x (maybe i (+1) (getMax <$> f x))

singleton :: Id -> AllocId
singleton (Id x i) y
  | x == y    = Just (Max i)
  | otherwise = Nothing

fromTerm :: Term -> AllocId
fromTerm (TVar x)            = singleton x
fromTerm TTrue              = mempty
fromTerm TFalse             = mempty
fromTerm (TBoolMatch e0 e1 e2) = fromTerm e0 <> fromTerm e1 <> fromTerm e2
fromTerm (TInt n)         = mempty
fromTerm (TPair e1 e2)       = fromTerm e1 <> fromTerm e2
fromTerm (TProdMatch e0 _ _ e1) = fromTerm e0 <> fromTerm e1
fromTerm (TNil _)            = mempty
fromTerm (TCons e1 e2)       = fromTerm e1 <> fromTerm e2
fromTerm (TListMatch e0 e1 _ _ e2) = fromTerm e0 <> fromTerm e1 <> fromTerm e2
fromTerm (TLam _ _ e)        = fromTerm e
fromTerm (TFix _ _ _ _ e)    = fromTerm e
fromTerm (TApp e1 e2)        = fromTerm e1 <> fromTerm e2
fromTerm (TBox _ _)          = mempty
fromTerm (TLetBox _ e1 e2) = fromTerm e1 <> fromTerm e2
fromTerm (TClo _ es)         = mconcat (fromTerm . snd <$> es)
fromTerm (TLet _ e1 e2)      = fromTerm e1 <> fromTerm e2
fromTerm (TPrimOp op)        = go op
  where
    go (IntEq e1 e2)  = fromTerm e1 <> fromTerm e2
    go (IntLe e1 e2)  = fromTerm e1 <> fromTerm e2
    go (IntLt e1 e2)  = fromTerm e1 <> fromTerm e2
    go (IntAdd e1 e2) = fromTerm e1 <> fromTerm e2
    go (IntSub e1 e2) = fromTerm e1 <> fromTerm e2
    go (IntMul e1 e2) = fromTerm e1 <> fromTerm e2
    go (IntDiv e1 e2) = fromTerm e1 <> fromTerm e2
    go (IntMod e1 e2) = fromTerm e1 <> fromTerm e2
    go (IntPow e1 e2) = fromTerm e1 <> fromTerm e2
    go (Inject e)     = fromTerm e

-- substitutions
-- renameVar :: (Id, Id) -> Term -> Term
-- renameVar (x, y) = subst (x, TVar y)

-- renameGVar :: (GId, GId) -> Term -> Term
-- renameGVar s e@(TVar _) = e
-- renameGVar s (Lam x t e) = Lam x t (renameGVar s e)
-- renameGVar s (App e1 e2) = App (renameGVar s e1) (renameGVar s e2)
-- renameGVar s (Box octx e) = Box octx (renameGVar s e)
-- renameGVar s@(u, v) e@(LetBox oectx w e1 e2)
--   | w == u    = e
--   | otherwise = LetBox oectx w (renameGVar s e1) (renameGVar s e2)
-- renameGVar s@(u, v) (Clo w es)
--   | w == u    = Clo v (renameGVar s <$> es)
--   | otherwise = Clo w (renameGVar s <$> es)

substv :: (Id, Value) -> Term -> Term
substv (x, v) = go
  where
    ev = liftToTerm v
    go = \case
      TVar y -> if y == x then ev else TVar y
      TTrue -> TTrue
      TFalse -> TFalse
      TBoolMatch e0 e1 e2 -> TBoolMatch (go e0) (go e1) (go e2)
      TInt n -> TInt n
      TPair e1 e2 -> TPair (go e1) (go e2)
      TProdMatch e0 y1 y2 e1 | y1 == x || y2 == x -> TProdMatch (go e0) y1 y2 e1
                             | otherwise          -> TProdMatch (go e0) y1 y2 (go e1)
      TNil t -> TNil t
      TCons e0 e1 -> TCons (go e0) (go e1)
      TListMatch e0 e1 y1 y2 e2 | y1 == x || y2 == x -> TListMatch (go e0) (go e1) y1 y2 e2
                               | otherwise          -> TListMatch (go e0) (go e1) y1 y2 (go e2)
      TLam y t e | y == x    -> TLam y t e
                | otherwise -> TLam y t (go e)
      TFix t1 t2 f y e | f == x || y == x -> TFix t1 t2 f y e
                      | otherwise -> TFix t1 t2 f y (go e)
      TApp e1 e2 -> TApp (go e1) (go e2)
      TBox octx e -> TBox octx e
      TLetBox u e1 e2 -> TLetBox u (go e1) (go e2)
      TClo u es -> TClo u (second go <$> es)
      TLet y e1 e2 | y == x    -> TLet y (go e1) e2
                  | otherwise -> TLet y (go e1) (go e2)
      TPrimOp op -> TPrimOp (go <$> op)

-- subst :: (Id, Term) -> Term -> Term
-- subst (x, e) (TVar y)
--   | y == x    = e
--   | otherwise = TVar y
-- subst s@(x, e) (Lam y t e1)
--   | y == x    = Lam y t e1
--   | otherwise =
--     let y' = newId (singleton x <> fromTerm e) y
--     in Lam y' t (subst s . renameVar (y, y') $ e1)
-- subst s (App e1 e2) = App (subst s e1) (subst s e2)
-- subst s e@(Box _ _) = e
-- subst s (LetBox oectx u e1 e2) = LetBox oectx u (subst s e1) (subst s e2)
-- subst s (Clo u es) = Clo u (subst s <$> es)

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
  | otherwise = TLetBox v (substGlobal s e1) (substGlobal s e2)
substGlobal s@(u,oectx,oe) e@(TClo v es)
  | v == u    = let es' = second (substGlobal s) <$> es
                in substSim (Map.fromList $ es') oe
  | otherwise = e
substGlobal s (TLet x e1 e2) = TLet x (substGlobal s e1) (substGlobal s e2)
substGlobal s (TPrimOp op) = TPrimOp (substGlobal s <$> op)

substSim :: Map Id Term -> Term -> Term
substSim s (TVar x) = Map.findWithDefault (error "simultaneous substitution failed") x s
substSim s TTrue = TTrue
substSim s TFalse = TFalse
substSim s (TBoolMatch e0 e1 e2) = TBoolMatch (substSim s e0) (substSim s e1) (substSim s e2)
substSim s (TInt n) = TInt n
substSim s (TPair e1 e2) = TPair (substSim s e1) (substSim s e2)
substSim s (TProdMatch e0 x y e1) =
  let a = foldMap fromTerm s
      ax = a <> singleton x
      x' = newId a x
      y' = newId ax y
      s' = Map.union (Map.fromList [(x, TVar x'), (y, TVar y')]) s
  in TProdMatch (substSim s e0) x' y' (substSim s' e1)
substSim s (TNil t) = TNil t
substSim s (TCons e1 e2) = TCons (substSim s e1) (substSim s e2)
substSim s (TListMatch e0 e1 x xs e2) =
  let a = foldMap fromTerm s
      ax = a <> singleton x
      x' = newId a x
      xs' = newId ax xs
      s' = Map.union (Map.fromList [(x, TVar x'), (xs, TVar xs')]) s
   in TListMatch (substSim s e0) (substSim s e1) x xs (substSim s' e2)
substSim s (TLam x t e) =
  let x' = newId (foldMap fromTerm s) x
      s' = Map.union (Map.singleton x (TVar x')) s
  in TLam x' t (substSim s' e)
substSim s (TFix t1 t2 f x e) =
  let a = foldMap fromTerm s
      af = a <> singleton f
      f' = newId a f
      x' = newId af x
      s' = Map.union (Map.fromList [(f, TVar f'), (x, TVar x')]) s
  in TFix t1 t2 f' x' (substSim s' e)
substSim s (TApp e1 e2) = TApp (substSim s e1) (substSim s e2)
substSim s e@(TBox _ _) = e
substSim s (TLetBox u e1 e2) = TLetBox u (substSim s e1) (substSim s e2)
substSim s (TClo u es) = TClo u (second (substSim s) <$> es)
substSim s (TLet x e1 e2) =
  let x' = newId (foldMap fromTerm s) x
      s' = Map.union (Map.singleton x (TVar x')) s
   in TLet x (substSim s e1) (substSim s' e2)
substSim s (TPrimOp op) = TPrimOp (substSim s <$> op)
