module Test where

import qualified Syntax.Object as O
import qualified Syntax.Meta as M
import Typing

-- three : [ f : Base -> Base |- Base -> Base ]
-- three = box[f. fn x -> f (f (f x))]
three :: M.Tm
three = M.Box [(f, btb)]
      $ O.Abs x O.Base
      $ (O.App vf $ O.App vf $ O.App vf vx)
  where
    btb = O.Arr O.Base O.Base
    f = O.Id "f"
    x = O.Id "x"
    vf = O.Var f
    vx = O.Var x

-- close : [ f : Base -> Base |- Base -> Base ] -> [ |- (Base -> Base) -> Base -> Base ]
-- close = fn c -> let box (g.U) = c in box[. fn f -> U with f]
close :: M.Tm
close = M.Abs c (M.BoxT [(f, btb)] btb)
      $ M.LetBox [g] u three
      $ M.Box []
      $ O.Abs f btb (O.Meta $ M.Inst u [vf])
  where
    btb = O.Arr O.Base O.Base
    c = M.Id "c"
    u = M.GId "U"
    g = O.Id "g"
    f = O.Id "f"
    vf = O.Var f

-- three' : [ |- (Base -> Base) -> Base -> Base ]
-- three' = close three
three' :: M.Tm
three' = M.App close three

-- inferType three
-- inferType close
-- inferType three'
