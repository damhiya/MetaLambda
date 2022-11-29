module MetaLambda.Examples where

import MetaLambda.Syntax
import MetaLambda.Typing

inferType' :: Mode -> Ctxs -> Term -> Either TypeError Type
inferType' = inferType

-- term1 :{0} [( f : Base -> Base, x : Base |- Base )]
-- term1 = return(lift(f,x. f (f (f x))))
term1 :: Term
term1 = Return (Lift ([] @> (f, btb) @> (x, b)) (
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
term2 :: Term
term2 = Return(Return(
          Lift [(f,btb)] (Lift [(x,b)] (
            Unlift vf [Unlift vf [Unlift vf [vx]]]
          ))
        ))
  where
    b = Base
    btb = Upshift [(x, b)] b
    f = Id "f" 0
    x = Id "x" 0
    vf = Var f
    vx = Var x

-- term3 :{0} [[( f : (x : Base |- Base), x : (|- Base) |- (|- Base) )]]
-- term3 = return(return( lift(f,x.lift(. f with (f with (f with (x with ()))) )) ))
term3 :: Term
term3 = Return(Return(
          Lift ([] @> (f,btb) @> (x,b')) (Lift [] (
            Unlift vf [Unlift vf [Unlift vf [Unlift vx []]]]
          ))
        ))
  where
    b = Base
    b' = Upshift [] b
    btb = Upshift [(x, b)] b
    f = Id "f" 0
    x = Id "x" 0
    vf = Var f
    vx = Var x

-- term4 :{0} [[( f : (x : Base |- Base) |- (x : Base |- Base) )]]
-- term4 = let return u = term2 in return(
--           let return v = u in return(
--             lift(f. v with (v with f) )
--         ))
term4 :: Term
term4 = LetReturn u term2 (Return (
          LetReturn v vu (Return (
            Lift [(f,btb)] (
              Unlift vv [Unlift vv [vf]]
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
    btb = Upshift [(x, b)] b

-- term5 :{0} [(x : Base |- Base)]
-- term5 = let return u = return(fn x -> x) in
--         let return v = return(lift(x.x)) in
--         return(lift(x. (u v) with x ))
term5 :: Term
term5 = LetReturn u (Return (Lam x btb (Var x))) (
        LetReturn v (Return (Lift [(x,b)] vx)) (
          Return (Lift [(x,b)] (Unlift (App vu vv) [vx]))
        ))
  where
    u = Id "u" 0
    v = Id "v" 0
    x = Id "x" 0
    vu = Var u
    vv = Var v
    vx = Var x
    b = Base
    btb = Upshift [(x,b)] b

-- term6 :{0} [(|- Base -> Base)] -> Base -> Base
-- term6 = let return u = return(lift(x.x)) in
--         fn z ->
--         let return v = z in
--         fn x -> (v with ()) (u with x)
term6 :: Term
term6 = LetReturn u (Return (Lift [(x,b)] vx)) (
        Lam z btb' (
        LetReturn v vz (
        Lam x b (
        App (Unlift vv []) (Unlift vu [vx])
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
    btb' = Downshift (Upshift [] btb)
