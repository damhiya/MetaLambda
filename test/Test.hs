module Main where

import           Data.Text                  (Text)
import qualified Data.Text                  as T
import qualified Data.Text.IO               as T
import           System.Directory
import           Test.Tasty
import qualified Test.Tasty.Golden.Advanced as Tasty

import           Equality
import           Parser.Lexer
import           Parser.Parser
import           PrettyPrinter
import           Reduction.Evaluation
import           Syntax

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

s1 :: Text
s1 = "fn (x : base) -> x"

s2 :: Text
s2 = "fn (c : [ f : base -> base |- base -> base ]) -> let box[f.U] = c in box[. fn (g : base -> base) -> U with (g)]"

s3 :: Text
s3 = "fn (n : [ f : base -> base |- base -> base]) -> let box[f.U] = n in box[f : base -> base. U with (U with (f))]"

goldenTestCase :: TestName ->
                  FilePath -> -- golden file
                  FilePath -> -- input file
                  (Text -> IO a) -> -- input processor
                  (Text -> IO a) -> -- golden processor
                  (a -> a -> Maybe String) -> -- compare input
                  (a -> Text) -> -- output processor
                  TestTree
goldenTestCase name golden input processInput processGolden cmp pretty =
  Tasty.goldenTest
    name
    (T.readFile golden >>= processGolden)
    (T.readFile input >>= processInput)
    (\x y -> pure (cmp x y))
    (\x -> T.writeFile golden (pretty x))

(</>) :: FilePath -> FilePath -> FilePath
xs </> ys = xs ++ "/" ++ ys

parseTerm :: String -> Text -> Either String Term
parseTerm f s = do
  case tokenize f s of
    Left err -> Left (show err)
    Right ts -> case parser ts of
      Left err -> Left (show err)
      Right e  -> Right e

main :: IO ()
main = do
  xs <- listDirectory inputPath
  let test = testGroup "golden testing" $ flip map xs $ \x ->
        goldenTestCase
          x
          (goldenPath </> x)
          (inputPath </> x)
          (pure . fmap eval . parseTerm (inputPath </> x))
          (pure . parseTerm x)
          (\mx my ->
             case mx of
               Left err -> Just ("golden parse error\n" ++ err)
               Right e1 -> case my of
                 Left err -> Just ("input parse error\n" ++ err)
                 Right e2 -> if eqAlpha e1 e2 then Nothing else Just "different!")
          (\case
              Left err -> T.pack err
              Right e -> T.pack $ show $ prettyTerm 1 e)
  defaultMain test
  where
    inputPath = "test/input"
    goldenPath = "test/golden"
