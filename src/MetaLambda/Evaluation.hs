module MetaLambda.Evaluation where

import           MetaLambda.Substitution
import           MetaLambda.Syntax

invalid :: Int -> Value
invalid n = error ("not type checked" ++ show n)

-- call by value evaluateuation
evaluate :: Term -> Value
evaluate (TVar _) = invalid 0
evaluate TTrue = VTrue
evaluate TFalse = VFalse
evaluate (TBoolMatch e0 e1 e2) =
  case evaluate e0 of
    VTrue  -> evaluate e1
    VFalse -> evaluate e2
    _      -> invalid 1
evaluate (TInt n) = VInt n
evaluate (TPair e1 e2) =
  let v1 = evaluate e1
      v2 = evaluate e2
  in VPair v1 v2
evaluate (TProdMatch e0 x y e1) =
  case evaluate e0 of
    VPair vx vy -> evaluate (applySubst [(y, liftToTerm vy), (x, liftToTerm vx)] e1)
    _           -> invalid 2
evaluate (TNil t) = VNil t
evaluate (TCons e1 e2) = VCons (evaluate e1) (evaluate e2)
evaluate (TListMatch e0 e1 x xs e2) =
  case evaluate e0 of
    VNil t       -> evaluate e1
    VCons vx vxs -> evaluate (applySubst [(xs, liftToTerm vxs), (x, liftToTerm vx)] e2)
    _            -> invalid 3
evaluate (TLam x t e) = VLam x t e
evaluate (TFix t1 t2 f x e) = VFix t1 t2 f x e
evaluate (TApp e1 e2) =
  let v1 = evaluate e1
      v2 = evaluate e2
  in case v1 of
       VLam x _ e3         -> evaluate (applySubst [(x, liftToTerm v2)] e3)
       vf@(VFix _ _ f x e) -> evaluate (applySubst [(x, liftToTerm v2), (f, liftToTerm vf)] e)
       _                   -> invalid 4
evaluate (TBox octx e) = VBox octx e
evaluate (TLetBox u e1 e2) =
  case evaluate e1 of
    VBox octx oe -> evaluate (applyGSubst (u, (erase octx, oe)) e2)
    _            -> invalid 5
evaluate (TClo _ _) = invalid 6
evaluate (TLet x e1 e2) =
  let v = evaluate e1
   in evaluate (applySubst [(x,liftToTerm v)] e2)
evaluate (TPrimOp op) = go op
  where
    go (IntEq e1 e2)  = binop (\m n -> if m == n then VTrue else VFalse) e1 e2
    go (IntLe e1 e2)  = binop (\m n -> if m <= n then VTrue else VFalse) e1 e2
    go (IntLt e1 e2)  = binop (\m n -> if m < n then VTrue else VFalse) e1 e2
    go (IntAdd e1 e2) = binop (\m n -> VInt (m + n)) e1 e2
    go (IntSub e1 e2) = binop (\m n -> VInt (m - n)) e1 e2
    go (IntMul e1 e2) = binop (\m n -> VInt (m * n)) e1 e2
    go (IntDiv e1 e2) = binop (\m n -> VInt (m `div` n)) e1 e2
    go (IntMod e1 e2) = binop (\m n -> VInt (m `mod` n)) e1 e2
    go (IntPow e1 e2) = binop (\m n -> VInt (m ^ n)) e1 e2
    go (Inject e) = case evaluate e of
                      VInt n -> VBox [] (TInt n)
                      _      -> invalid 7
    binop f e1 e2 =
      case (evaluate e1, evaluate e2) of
        (VInt m, VInt n) -> f m n
        _                -> invalid 8
