module MetaLambda.Substitution where

import           Data.Map       (Map)
import qualified Data.Map       as Map
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
fromTerm (Var x)            = singleton x
fromTerm BTrue              = mempty
fromTerm BFalse             = mempty
fromTerm (BoolMatch e0 e1 e2) = fromTerm e0 <> fromTerm e1 <> fromTerm e2
fromTerm (IntLit n)         = mempty
fromTerm (Pair e1 e2)       = fromTerm e1 <> fromTerm e2
fromTerm (ProdMatch e0 _ _ e1) = fromTerm e0 <> fromTerm e1
fromTerm (Nil _)            = mempty
fromTerm (Cons e1 e2)       = fromTerm e1 <> fromTerm e2
fromTerm (ListMatch e0 e1 _ _ e2) = fromTerm e0 <> fromTerm e1 <> fromTerm e2
fromTerm (Lam _ _ e)        = fromTerm e
fromTerm (Fix _ _ _ _ e)    = fromTerm e
fromTerm (App e1 e2)        = fromTerm e1 <> fromTerm e2
fromTerm (Box _ _)          = mempty
fromTerm (LetBox _ _ e1 e2) = fromTerm e1 <> fromTerm e2
fromTerm (Clo _ es)         = mconcat (map fromTerm es)
fromTerm (Let _ e1 e2)      = fromTerm e1 <> fromTerm e2
fromTerm (PrimOp op)        = go op
  where
    go (IntEq e1 e2) = fromTerm e1 <> fromTerm e2
    go (IntLe e1 e2) = fromTerm e1 <> fromTerm e2
    go (IntAdd e1 e2) = fromTerm e1 <> fromTerm e2
    go (IntSub e1 e2) = fromTerm e1 <> fromTerm e2
    go (IntMul e1 e2) = fromTerm e1 <> fromTerm e2
    go (IntDiv e1 e2) = fromTerm e1 <> fromTerm e2
    go (IntMod e1 e2) = fromTerm e1 <> fromTerm e2
    go (IntPow e1 e2) = fromTerm e1 <> fromTerm e2
    go (Inject e) = fromTerm e

-- substitutions
-- renameVar :: (Id, Id) -> Term -> Term
-- renameVar (x, y) = subst (x, Var y)

-- renameGVar :: (GId, GId) -> Term -> Term
-- renameGVar s e@(Var _) = e
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
      Var y -> if y == x then ev else Var y
      BTrue -> BTrue
      BFalse -> BFalse
      BoolMatch e0 e1 e2 -> BoolMatch (go e0) (go e1) (go e2)
      IntLit n -> IntLit n
      Pair e1 e2 -> Pair (go e1) (go e2)
      ProdMatch e0 y1 y2 e1 | y1 == x || y2 == x -> ProdMatch (go e0) y1 y2 e1
                            | otherwise          -> ProdMatch (go e0) y1 y2 (go e1)
      Nil t -> Nil t
      Cons e0 e1 -> Cons (go e0) (go e1)
      ListMatch e0 e1 y1 y2 e2 | y1 == x || y2 == x -> ListMatch (go e0) (go e1) y1 y2 e2
                               | otherwise          -> ListMatch (go e0) (go e1) y1 y2 (go e2)
      Lam y t e | y == x    -> Lam y t e
                | otherwise -> Lam y t (go e)
      Fix t1 t2 f y e | y == x -> Fix t1 t2 f y e
                      | otherwise -> Fix t1 t2 f y (go e)
      App e1 e2 -> App (go e1) (go e2)
      Box octx e -> Box octx e
      LetBox oectx u e1 e2 -> LetBox oectx u (go e1) (go e2)
      Clo u es -> Clo u (go <$> es)
      Let y e1 e2 | y == x    -> Let y (go e1) e2
                  | otherwise -> Let y (go e1) (go e2)
      PrimOp op -> PrimOp (go <$> op)

-- subst :: (Id, Term) -> Term -> Term
-- subst (x, e) (Var y)
--   | y == x    = e
--   | otherwise = Var y
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
substGlobal s e@(Var _) = e
substGlobal s BTrue = BTrue
substGlobal s BFalse = BFalse
substGlobal s (BoolMatch e0 e1 e2) = BoolMatch (substGlobal s e0) (substGlobal s e1) (substGlobal s e2)
substGlobal s (IntLit n) = IntLit n
substGlobal s (Pair e1 e2) = Pair (substGlobal s e1) (substGlobal s e2)
substGlobal s (ProdMatch e0 x y e1) = ProdMatch (substGlobal s e0) x y (substGlobal s e1)
substGlobal s (Nil t) = Nil t
substGlobal s (Cons e0 e1) = Cons (substGlobal s e0) (substGlobal s e1)
substGlobal s (ListMatch e0 e1 x xs e2) = ListMatch (substGlobal s e0) (substGlobal s e1) x xs (substGlobal s e2)
substGlobal s (Lam x t e) = Lam x t (substGlobal s e)
substGlobal s (Fix t1 t2 f x e) = Fix t1 t2 f x (substGlobal s e)
substGlobal s (App e1 e2) = App (substGlobal s e1) (substGlobal s e2)
substGlobal s (Box octx oe) = Box octx (substGlobal s oe)
substGlobal s@(u, _, _) (LetBox oectx v e1 e2)
  | v == u    = LetBox oectx v (substGlobal s e1) e2
  | otherwise = LetBox oectx v (substGlobal s e1) (substGlobal s e2)
substGlobal s@(u,oectx,oe) e@(Clo v es)
  | v == u    = let es' = map (substGlobal s) es
                in substSim (Map.fromList $ zip oectx es') oe
  | otherwise = e
substGlobal s (Let x e1 e2) = Let x (substGlobal s e1) (substGlobal s e2)
substGlobal s (PrimOp op) = PrimOp (substGlobal s <$> op)

substSim :: Map Id Term -> Term -> Term
substSim s (Var x) = Map.findWithDefault (error "simultaneous substitution failed") x s
substSim s BTrue = BTrue
substSim s BFalse = BFalse
substSim s (BoolMatch e0 e1 e2) = BoolMatch (substSim s e0) (substSim s e1) (substSim s e2)
substSim s (IntLit n) = IntLit n
substSim s (Pair e1 e2) = Pair (substSim s e1) (substSim s e2)
substSim s (ProdMatch e0 x y e1) =
  let a = foldMap fromTerm s
      ax = a <> singleton x
      x' = newId a x
      y' = newId ax y
      s' = Map.union (Map.fromList [(x, Var x'), (y, Var y')]) s
  in ProdMatch (substSim s e0) x' y' (substSim s' e1)
substSim s (Nil t) = Nil t
substSim s (Cons e1 e2) = Cons (substSim s e1) (substSim s e2)
substSim s (ListMatch e0 e1 x xs e2) =
  let a = foldMap fromTerm s
      ax = a <> singleton x
      x' = newId a x
      xs' = newId ax xs
      s' = Map.union (Map.fromList [(x, Var x'), (xs, Var xs')]) s
   in ListMatch (substSim s e0) (substSim s e1) x xs (substSim s' e2)
substSim s (Lam x t e) =
  let x' = newId (foldMap fromTerm s) x
      s' = Map.union (Map.singleton x (Var x')) s
  in Lam x' t (substSim s' e)
substSim s (Fix t1 t2 f x e) =
  let a = foldMap fromTerm s
      af = a <> singleton f
      f' = newId a f
      x' = newId af x
      s' = Map.union (Map.fromList [(f, Var f'), (x, Var x')]) s
  in Fix t1 t2 f' x' (substSim s' e)
substSim s (App e1 e2) = App (substSim s e1) (substSim s e2)
substSim s e@(Box _ _) = e
substSim s (LetBox oectx u e1 e2) = LetBox oectx u (substSim s e1) (substSim s e2)
substSim s (Clo u es) = Clo u (substSim s <$> es)
substSim s (Let x e1 e2) =
  let x' = newId (foldMap fromTerm s) x
      s' = Map.union (Map.singleton x (Var x')) s
   in Let x (substSim s e1) (substSim s' e2)
substSim s (PrimOp op) = PrimOp (substSim s <$> op)
