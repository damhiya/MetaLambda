module MetaLambda.ConcreteSyntax.PrettyPrinter where

import           Prettyprinter
import           MetaLambda.Syntax

pair :: Doc ann -> Doc ann -> Doc ann
pair x y = parens $ hsep [x, comma, y]

wrap :: Int -> Int -> Doc ann -> Doc ann
wrap p n doc
  | p > n     = parens doc
  | otherwise = doc

prettyId :: Id -> Doc ann
prettyId (Id x i) = pretty $ mconcat [x,"_",show i]

prettyGId :: GId -> Doc ann
prettyGId (GId u i) = pretty $ mconcat [u,"_",show i]

typing :: Id -> Type -> Doc ann
typing x t = hsep [prettyId x, colon, prettyType 1 t]

prettyTerm :: Int -> Term -> Doc ann
prettyTerm _ (Var x) = prettyId x
prettyTerm _ BTrue = "true"
prettyTerm _ BFalse = "false"
prettyTerm _ (BoolMatch e0 e1 e2) =
  hsep [ "match", prettyTerm 2 e0, "with"
       , "|", "true",  "->", prettyTerm 1 e1
       , "|", "false", "->", prettyTerm 1 e2
       , "end"
       ]
prettyTerm _ (IntLit n) = pretty n
prettyTerm _ (Pair e1 e2) = pair (prettyTerm 1 e1) (prettyTerm 1 e2)
prettyTerm _ (ProdMatch e0 x y e1) =
  hsep [ "match", prettyTerm 2 e0, "with"
       , "|",  pair (prettyId x) (prettyId y), "->", prettyTerm 1 e1
       , "end"
       ]
prettyTerm p (Nil t) = wrap p 10 $ hsep [ "[]", "of", prettyType 3 t ]
prettyTerm p (Cons e1 e2) = wrap p 5 $ hsep [ prettyTerm 6 e1, "::", prettyTerm 5 e2 ]
prettyTerm _ (ListMatch e0 e1 x xs e2) =
  hsep [ "match", prettyTerm 2 e0, "with"
       , "|", "[]", "->", prettyTerm 1 e1
       , "|", prettyId x, "::", prettyId xs, "->", prettyTerm 1 e2
       ]
prettyTerm p (Lam x t e) =
  wrap p 1 $ hsep [ "fn"
                  , parens $ typing x t
                  , "->"
                  , prettyTerm 1 e
                  ]
prettyTerm p (Fix t1 t2 f x e) =
  wrap p 1 $ hsep [ "fix"
                  , parens $ typing f (Arr t1 t2)
                  , prettyId x
                  , "->"
                  , prettyTerm 1 e
                  ]
prettyTerm p (App t1 t2) = wrap p 2 $ prettyTerm 2 t1 <+> prettyTerm 3 t2
prettyTerm _ (Box ctx t) = "box" <> brackets (prettyLCtx ctx <+> dot <+> prettyTerm 1 t)
prettyTerm p (LetBox ectx u e1 e2) =
  wrap p 1 $ hsep [ "let"
                  , "box" <> brackets (prettyLECtx ectx <+> dot <+> prettyGId u)
                  , "="
                  , prettyTerm 1 e1
                  , "in"
                  , prettyTerm 1 e2
                  ]
prettyTerm p (Clo u es) = wrap p 1 $ hsep [prettyGId u, "with", tupled (map (prettyTerm 1) es)]
prettyTerm p (Let x e1 e2) =
  wrap p 1 $ hsep [ "let"
                  , prettyId x
                  , "="
                  , prettyTerm 1 e1
                  , "in"
                  , prettyTerm 1 e2
                  ]
prettyTerm p (PrimOp op) = go op
  where
    go (IntEq e1 e2)  = wrap p 4 $ hsep [ prettyTerm 5 e1, "==", prettyTerm 5 e2 ]
    go (IntLe e1 e2)  = wrap p 4 $ hsep [ prettyTerm 5 e1, "<=", prettyTerm 5 e2 ]
    go (IntAdd e1 e2) = wrap p 6 $ hsep [ prettyTerm 6 e1, "+", prettyTerm 7 e2 ]
    go (IntSub e1 e2) = wrap p 6 $ hsep [ prettyTerm 6 e1, "-", prettyTerm 7 e2 ]
    go (IntMul e1 e2) = wrap p 7 $ hsep [ prettyTerm 7 e1, "*", prettyTerm 8 e2 ]
    go (IntDiv e1 e2) = wrap p 7 $ hsep [ prettyTerm 7 e1, "/", prettyTerm 8 e2 ]
    go (IntMod e1 e2) = wrap p 7 $ hsep [ prettyTerm 7 e1, "%", prettyTerm 8 e2 ]
    go (IntPow e1 e2) = wrap p 8 $ hsep [ prettyTerm 9 e1, "^", prettyTerm 8 e2 ]
    go (Inject e)     = wrap p 9 $ hsep [ "inject", prettyTerm 10 e ]

prettyType :: Int -> Type -> Doc ann
prettyType _ Base = "base"
prettyType _ Bool = "bool"
prettyType _ Int = "int"
prettyType p (Prod t1 t2) = wrap p 2 $ hsep [ prettyType 2 t1, "*", prettyType 3 t2 ]
prettyType _ (List t) = hsep [ "list", prettyType 3 t ]
prettyType p (Arr t1 t2) =
  wrap p 1 $ hsep [ prettyType 2 t1
                  , "->"
                  , prettyType 1 t2
                  ]
prettyType _ (BoxT ctx t) = brackets $
  hsep [ prettyLCtx ctx
       , "|-"
       , prettyType 1 t
       ]

prettyLCtx :: LCtx -> Doc ann
prettyLCtx ctx = hsep $ punctuate comma $ map (uncurry typing) $ reverse ctx

prettyLECtx :: LECtx -> Doc ann
prettyLECtx ectx = hsep $ punctuate comma $ map prettyId $ reverse ectx
