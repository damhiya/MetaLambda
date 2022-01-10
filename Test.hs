module Test where

import qualified Syntax.Object as O
import qualified Syntax.Meta as M
import Typing

-- close : [ f : Base -> Base |- Base -> Base ] -> [ |- (Base -> Base) -> Base -> Base ]
-- close = fn c -> let box (g.U) = c in box(. fn f -> U with f)
close :: M.Tm
close = M.Abs c (M.BoxT [(f, btb)] btb)
      $ M.LetBox [g] u vc
      $ M.Box []
      $ O.Abs f btb (O.Meta $ M.Inst u [vf])
  where
    btb = O.Arr O.Base O.Base
    c = M.Id "c"
    u = M.GId "U"
    g = O.Id "g"
    f = O.Id "f"
    vc = M.Var c
    vf = O.Var f

-- three : [ f : Base -> Base, x : Base |- Base ]
-- three = box(f x. f (f (f x)))
three :: M.Tm
three = M.Box [(x, b), (f, btb)]
      $ (O.App vf $ O.App vf $ O.App vf vx)
  where
    btb = O.Arr O.Base O.Base
    b = O.Base
    f = O.Id "f"
    x = O.Id "x"
    vf = O.Var f
    vx = O.Var x

-- three' : [ f : Base -> Base |- Base -> Base ]
-- three' = let box(f,x.U) = three in box(f. fn x -> U with (f,x))
three' :: M.Tm
three' = M.LetBox [x,f] u three
       $ M.Box [(f, btb)]
       $ O.Abs x O.Base (O.Meta $ M.Inst u [vx,vf])
  where
    btb = O.Arr O.Base O.Base
    u = M.GId "U"
    f = O.Id "f"
    x = O.Id "x"
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
