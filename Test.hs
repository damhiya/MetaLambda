module Test where

import qualified Syntax.Object as O
import qualified Syntax.Meta as M
import Typing

-- three : [ f : Base -> Base |- Base -> Base ]
-- three = box[f. fn x -> f (f (f x))]
three :: M.Tm
three = M.Box [(f, O.Arr O.Base O.Base)] (O.Abs x O.Base
                                         (O.App vf (O.App vf (O.App vf vx))))
  where
    f = O.Id "f"
    x = O.Id "x"
    vf = O.Var f
    vx = O.Var x

-- three' : [ |- (Base -> Base) -> Base -> Base ]
-- three' = let box (g.U) = three in box(. fn f -> U with f)
three' :: M.Tm
three' = M.LetBox [g] u three (M.Box [] (O.Abs f (O.Arr O.Base O.Base)
                                        (O.Meta (M.Inst u [O.Var f]))))
  where
    g = O.Id "g"
    f = O.Id "f"
    u = M.GId "U"

tcThree :: Maybe M.Ty
tcThree = inferMeta [] [] three

tcThree' :: Maybe M.Ty
tcThree' = inferMeta [] [] three'
