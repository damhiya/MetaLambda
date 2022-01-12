module Test where

import qualified Syntax.Object as O
import qualified Syntax.Meta as M
import Typing
import Reduction.Normalization

-- close : [ f : Base -> Base |- Base -> Base ] -> [ |- (Base -> Base) -> Base -> Base ]
-- close = fn c -> let box (f.U) = c in box(. fn g -> U with g)
close :: M.Tm
close = M.Abs c (M.BoxT [(f, btb)] btb)
      $ M.LetBox [f] u vc
      $ M.Box []
      $ O.Abs g btb (O.Meta $ M.Inst u [vg])
  where
    btb = O.Arr O.Base O.Base
    c = M.Id "c" 0
    vc = M.Var c
    u = M.GId "U" 0
    f = O.Id "f" 0
    g = O.Id "g" 0
    vg = O.Var g

-- three : [ f : Base -> Base, x : Base |- Base ]
-- three = box(f x. f (f (f x)))
three :: M.Tm
three = M.Box [(x, b), (f, btb)]
      $ (O.App vf $ O.App vf $ O.App vf vx)
  where
    btb = O.Arr O.Base O.Base
    b = O.Base
    f = O.Id "f" 0
    x = O.Id "x" 0
    vf = O.Var f
    vx = O.Var x

-- three' : [ f : Base -> Base |- Base -> Base ]
-- three' = let box(f,x.U) = three in box(f. fn x -> U with (f,x))
three' :: M.Tm
three' = M.LetBox [x,f] u three
       $ M.Box [(f, btb)]
       $ O.Abs x b (O.Meta $ M.Inst u [vx,vf])
  where
    btb = O.Arr O.Base O.Base
    b = O.Base
    u = M.GId "U" 0
    f = O.Id "f" 0
    x = O.Id "x" 0
    vf = O.Var f
    vx = O.Var x

-- three'' : [ |- (Base -> Base) -> Base -> Base ]
-- three'' = close three
three'' :: M.Tm
three'' = M.App close three'

-- nine' : [ f : Base -> Base |- Base -> Base ]
-- nine' = let box (f.U) = three' in box(f. U with (U with f))
nine' :: M.Tm
nine' = M.LetBox [f] u three'
      $ M.Box [(f, btb)]
      $ O.Meta (M.Inst u [O.Meta (M.Inst u [vf])])
  where
    btb = O.Arr O.Base O.Base
    f = O.Id "f" 0
    vf = O.Var f
    u = M.GId "U" 0
-- nine' -->
--   box(f. fn y ->
--            ((fn x -> f (f (f x)))
--              ((fn x -> f (f (f x)))
--                ((fn x -> f (f (f x))) y))))

-- nine'' : [ |- (Base -> Base) -> Base -> Base ]
-- nine'' = close nine'
nine'' :: M.Tm
nine'' = M.App close nine'

-- try these
_ = inferType close
_ = inferType three
_ = inferType three'
_ = inferType three''
_ = inferType nine'
_ = inferType nine''

_ = normalize close
_ = normalize three
_ = normalize three'
_ = normalize three''
_ = normalize nine'
_ = normalize nine''
