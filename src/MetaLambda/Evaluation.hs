module MetaLambda.Evaluation where

import           MetaLambda.Substitution
import           MetaLambda.Syntax

invalid :: Value
invalid = error "not type checked"

-- call by value evaluation
eval :: Term -> Value
eval (Var _) = invalid
eval BTrue = VTrue
eval BFalse = VFalse
eval (BoolMatch e0 e1 e2) =
  case eval e0 of
    VTrue  -> eval e1
    VFalse -> eval e2
    _      -> invalid
eval (IntLit n) = VInt n
eval (Pair e1 e2) =
  let v1 = eval e1
      v2 = eval e2
  in VPair v1 v2
eval (ProdMatch e0 x y e1) =
  case eval e0 of
    VPair vx vy -> eval (substv (x, vx) . substv (y,vy) $ e1)
    _           -> invalid
eval (Nil t) = VNil t
eval (Cons e1 e2) = VCons (eval e1) (eval e2)
eval (ListMatch e0 e1 x xs e2) =
  case eval e0 of
    VNil t       -> eval e1
    VCons vx vxs -> eval (substv (x, vx) . substv (xs,vxs) $ e2)
    _            -> invalid
eval (Lam x t e) = VLam x t e
eval (Fix t1 t2 f x e) = VFix t1 t2 f x e
eval (App e1 e2) =
  let v1 = eval e1
      v2 = eval e2
  in case v1 of
       VLam x _ e3         -> eval (substv (x, v2) e3)
       vf@(VFix _ _ f x e) -> eval (substv (f,vf) . substv (x,v2) $ e)
       _                   -> invalid
eval (Box octx e) = VBox octx e
eval (LetBox oectx u e1 e2) =
  case eval e1 of
    VBox octx oe -> eval (substGlobal (u, oectx, oe) e2)
    _           -> invalid
eval (Clo _ _) = invalid
eval (PrimOp op) = go op
  where
    go (IntEq e1 e2)  = binop (\m n -> if m == n then VTrue else VFalse) e1 e2
    go (IntLe e1 e2)  = binop (\m n -> if m <= n then VTrue else VFalse) e1 e2
    go (IntAdd e1 e2) = binop (\m n -> VInt (m + n)) e1 e2
    go (IntSub e1 e2) = binop (\m n -> VInt (m - n)) e1 e2
    go (IntMul e1 e2) = binop (\m n -> VInt (m * n)) e1 e2
    go (IntDiv e1 e2) = binop (\m n -> VInt (m `div` n)) e1 e2
    go (IntMod e1 e2) = binop (\m n -> VInt (m `mod` n)) e1 e2
    go (IntPow e1 e2) = binop (\m n -> VInt (m ^ n)) e1 e2
    go (Inject e) = case eval e of
                      VInt n -> VBox [] (IntLit n)
                      _ -> undefined
    binop f e1 e2 =
      case (eval e1, eval e2) of
        (VInt m, VInt n) -> f m n
        _ -> invalid
