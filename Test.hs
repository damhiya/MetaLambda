module Test where

import Syntax
import Typing
import Reduction.Evaluation

-- close : [ f : Base -> Base |- Base -> Base ] -> [ |- (Base -> Base) -> Base -> Base ]
-- close = fn c -> let box (f.U) = c in box(. fn g -> U with g)
close :: Term
close = Lam c (BoxT [(f, btb)] btb)
      $ LetBox [f] u vc
      $ Box []
      $ Lam g btb (Clo u [vg])
  where
    btb = Arr Base Base
    u = GId "U" 0
    c = Id "c" 0
    f = Id "f" 0
    g = Id "g" 0
    vc = Var c
    vg = Var g

-- three : [ f : Base -> Base, x : Base |- Base ]
-- three = box(f x. f (f (f x)))
three :: Term
three = Box [(x, b), (f, btb)] (App vf $ App vf $ App vf vx)
  where
    b = Base
    btb = Arr Base Base
    f = Id "f" 0
    x = Id "x" 0
    vf = Var f
    vx = Var x

-- three' : [ f : Base -> Base |- Base -> Base ]
-- three' = let box(f,x.U) = three in box(f. fn x -> U with (f,x))
three' :: Term
three' = LetBox [x,f] u three
       $ Box [(f, btb)]
       $ Lam x b (Clo u [vx,vf])
  where
    b = Base
    btb = Arr Base Base
    u = GId "U" 0
    f = Id "f" 0
    x = Id "x" 0
    vf = Var f
    vx = Var x

-- three'' : [ |- (Base -> Base) -> Base -> Base ]
-- three'' = close three
three'' :: Term
three'' = App close three'

-- nine' : [ f : Base -> Base |- Base -> Base ]
-- nine' = let box (f.U) = three' in box(f. U with (U with f))
nine' :: Term
nine' = LetBox [f] u three'
      $ Box [(f, btb)]
      $ Clo u [Clo u [vf]]
  where
    btb = Arr Base Base
    u = GId "U" 0
    f = Id "f" 0
    vf = Var f
-- nine' -->
--   box(f. fn y ->
--            ((fn x -> f (f (f x)))
--              ((fn x -> f (f (f x)))
--                ((fn x -> f (f (f x))) y))))

-- nine'' : [ |- (Base -> Base) -> Base -> Base ]
-- nine'' = close nine'
nine'' :: Term
nine'' = App close nine'

-- try these
_ = inferType [] [] close
_ = inferType [] [] three
_ = inferType [] [] three'
_ = inferType [] [] three''
_ = inferType [] [] nine'
_ = inferType [] [] nine''

_ = eval close
_ = eval three
_ = eval three'
_ = eval three''
_ = eval nine'
_ = eval nine''
