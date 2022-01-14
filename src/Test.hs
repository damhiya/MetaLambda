module Test where

import           Reduction.Evaluation
import           Syntax
import           Typing

import           Data.Text
import           Parser.Lexer
import           Parser.Parser
import qualified Text.Megaparsec      as P

-- close : [ f : Base -> Base |- Base -> Base ] -> [ |- (Base -> Base) -> Base -> Base ]
-- close = fn c -> let box (f.U) = c in box(. fn g -> U with g)
close :: Term
close = Lam c (BoxT [(f, btb)] btb)
      $ LetBox [f] u vc
      $ Box []
      $ Lam g btb (Clo u [vg])
  where
    b = Base
    btb = Arr b b
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
    btb = Arr b b
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
    btb = Arr b b
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
    b = Base
    btb = Arr b b
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

-- ex4 : [ x : A |- B ] -> [ y1 : A |- [ y2 : B |- C ] ] -> [ z : A |- C ]
-- ex4 =
--   fn c1 c2 ->
--     let box(x.u) = c1 in
--     let box(y1.v) = c2 in
--       box(z.
--         let box(y2.w) = v with z in
--           w with (u with z))
ex4 :: Term
ex4 = Lam c1 (BoxT [(x,a)] b)
    $ Lam c2 (BoxT [(y1,a)] (BoxT [(y2,b)] c))
    $ LetBox [x] u (Var c1)
    $ LetBox [y1] v (Var c2)
    $ Box [(z,a)]
    $ LetBox [y2] w (Clo v [Var z])
    $ Clo w [Clo u [Var z]]
  where
    a = Base
    b = Arr Base Base
    c = Arr Base (Arr Base Base)
    u = GId "u" 0
    v = GId "v" 0
    w = GId "w" 0
    c1 = Id "c1" 0
    c2 = Id "c2" 0
    x = Id "x" 0
    y1 = Id "y1" 0
    y2 = Id "y2" 0
    z = Id "z" 0

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

-- parsing test
parse :: Text -> IO Term
parse s =
  case tokenize "" s of
    Left b -> putStr (P.errorBundlePretty b) *> fail "lexing error"
    Right ts -> do
      let (es, r) = parser ts
      case es of
        [e] -> pure e
        _ -> do
          print es
          print r
          fail "parsing error"

s1 :: Text
s1 = "fn (x : base) -> x"

s2 :: Text
s2 = "fn (c : [ f : base -> base |- base -> base ]) -> let box[f.U] = c in box[. fn (g : base -> base) -> U with (g)]"

s3 :: Text
s3 = "fn (n : [ f : base -> base |- base -> base]) -> let box[f.U] = n in box[f : base -> base. U with (U with (f))]"
