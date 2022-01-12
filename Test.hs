module Test where

import qualified Syntax.Object as O
import qualified Syntax.Meta as M
import Typing

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

-- try these
_ = inferType close
_ = inferType three
_ = inferType three'
_ = inferType three''
