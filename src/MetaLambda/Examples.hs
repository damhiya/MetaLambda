module MetaLambda.Examples where

import MetaLambda.Syntax
import MetaLambda.Typing
import MetaLambda.Evaluation

inferType' :: StdMode -> Ctxs StdMode -> Term StdMode -> Either TypeError (Type StdMode)
inferType' = inferType

-- term1 :{0} [( f : Base -> Base, x : Base |- Base )]
-- term1 = return(lift(f,x. f (f (f x))))
term1 :: Term StdMode
term1 = Return 1 (Lift 0 ([] @> (f, btb) @> (x, b)) (
          App vf (App vf (App vf vx))
        ))
  where
    b = Base
    btb = Arr Base Base
    f = Id "f" 0
    x = Id "x" 0
    vf = Var f
    vx = Var x

-- term2 :{0} [[( f : (x : Base |- Base) |- (x : Base |- Base) )]]
-- term2 = return(return( lift(f.lift(x. f with (f with (f with x)) )) ))
term2 :: Term StdMode
term2 = Return 1 (Return 2 (
          Lift 1 [(f,btb)] (Lift 0 [(x,b)] (
            Unlift 1 vf [Unlift 1 vf [Unlift 1 vf [vx]]]
          ))
        ))
  where
    b = Base
    btb = Upshift 0 [(x, b)] b
    f = Id "f" 0
    x = Id "x" 0
    vf = Var f
    vx = Var x

-- term3 :{0} [[( f : (x : Base |- Base), x : (|- Base) |- (|- Base) )]]
-- term3 = return(return( lift(f,x.lift(. f with (f with (f with (x with ()))) )) ))
term3 :: Term StdMode
term3 = Return 1 (Return 2 (
          Lift 1 ([] @> (f,btb) @> (x,b')) (Lift 0 [] (
            Unlift 1 vf [Unlift 1 vf [Unlift 1 vf [Unlift 1 vx []]]]
          ))
        ))
  where
    b = Base
    b' = Upshift 0 [] b
    btb = Upshift 0 [(x, b)] b
    f = Id "f" 0
    x = Id "x" 0
    vf = Var f
    vx = Var x

-- term4 :{0} [[( f : (x : Base |- Base) |- (x : Base |- Base) )]]
-- term4 = let return u = term2 in return(
--           let return v = u in return(
--             lift(f. v with (v with f) )
--         ))
term4 :: Term StdMode
term4 = LetReturn 1 u term2 (Return 1 (
          LetReturn 2 v vu (Return 2 (
            Lift 1 [(f,btb)] (
              Unlift 2 vv [Unlift 2 vv [vf]]
            )
          ))
        ))
  where
    u = Id "u" 0
    v = Id "v" 0
    f = Id "f" 0
    x = Id "x" 0
    vu = Var u
    vv = Var v
    vf = Var f
    b = Base
    btb = Upshift 0 [(x, b)] b

-- term5 :{0} [(x : Base |- Base)]
-- term5 = let return u = return(fn x -> x) in
--         let return v = return(lift(x.x)) in
--         return(lift(x. (u v) with x ))
term5 :: Term StdMode
term5 = LetReturn 1 u (Return 1 (Lam x btb (Var x))) (
        LetReturn 1 v (Return 1 (Lift 0 [(x,b)] vx)) (
          Return 1 (Lift 0 [(x,b)] (Unlift 1 (App vu vv) [vx]))
        ))
  where
    u = Id "u" 0
    v = Id "v" 0
    x = Id "x" 0
    vu = Var u
    vv = Var v
    vx = Var x
    b = Base
    btb = Upshift 0 [(x,b)] b

-- term6 :{0} [(|- Base -> Base)] -> Base -> Base
-- term6 = let return u = return(lift(x.x)) in
--         fn z ->
--         let return v = z in
--         fn x -> (v with ()) (u with x)
term6 :: Term StdMode
term6 = LetReturn 1 u (Return 1 (Lift 0 [(x,b)] vx)) (
        Lam z btb' (
        LetReturn 1 v vz (
        Lam x b (
        App (Unlift 1 vv []) (Unlift 1 vu [vx])
        ))))
  where
    u = Id "u" 0
    z = Id "z" 0
    v = Id "v" 0
    x = Id "x" 0
    vu = Var u
    vz = Var z
    vv = Var v
    vx = Var x
    b = Base
    btb = Arr b b
    btb' = Downshift 1 (Upshift 0 [] btb)
