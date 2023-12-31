module MetaLambda.ConcreteSyntax.PrettyPrinter where

import           MetaLambda.Syntax
import           Prettyprinter

pair :: Doc ann -> Doc ann -> Doc ann
pair x y = parens $ hsep [x, comma, y]

wrap :: Int -> Int -> Doc ann -> Doc ann
wrap p n doc
  | p > n     = parens doc
  | otherwise = doc

prettyId :: Id -> Doc ann
prettyId (Id x 0) = pretty $ x
prettyId (Id x i) = pretty $ mconcat [x,"_",show i]

prettyGId :: GId -> Doc ann
prettyGId (GId u 0) = pretty $ u
prettyGId (GId u i) = pretty $ mconcat [u,"_",show i]

typing :: Id -> Type -> Doc ann
typing x t = hsep [prettyId x, colon, prettyType 1 t]

prettyTerm :: Int -> Term -> Doc ann
prettyTerm _ (TVar x) = prettyId x
prettyTerm _ TTrue = "true"
prettyTerm _ TFalse = "false"
prettyTerm _ (TBoolMatch e0 e1 e2) =
  hsep [ "if", prettyTerm 2 e0, "then", prettyTerm 1 e1, "else", prettyTerm 2 e2 ]
prettyTerm _ (TInt n) = pretty n
prettyTerm _ (TPair e1 e2) = pair (prettyTerm 1 e1) (prettyTerm 1 e2)
prettyTerm _ (TProdMatch e0 x y e1) =
  hsep [ "match", prettyTerm 2 e0, "with"
       , "|",  pair (prettyId x) (prettyId y), "->", prettyTerm 1 e1
       , "end"
       ]
prettyTerm p (TNil t) = wrap p 10 $ hsep [ "[]", "of", prettyType 3 t ]
prettyTerm p (TCons e1 e2) = wrap p 5 $ hsep [ prettyTerm 6 e1, "::", prettyTerm 5 e2 ]
prettyTerm _ (TListMatch e0 e1 x xs e2) =
  hsep [ "match", prettyTerm 2 e0, "with"
       , "|", "[]", "->", prettyTerm 1 e1
       , "|", prettyId x, "::", prettyId xs, "->", prettyTerm 1 e2
       ]
prettyTerm p (TLam x t e) =
  wrap p 1 $ hsep [ "fn"
                  , parens $ typing x t
                  , "->"
                  , prettyTerm 1 e
                  ]
prettyTerm p (TFix t1 t2 f x e) =
  wrap p 1 $ hsep [ "fix"
                  , parens $ typing f (Arr t1 t2)
                  , prettyId x
                  , "->"
                  , prettyTerm 1 e
                  ]
prettyTerm p (TApp t1 t2) = wrap p 2 $ prettyTerm 2 t1 <+> prettyTerm 3 t2
prettyTerm _ (TBox ctx t) = "box" <> brackets (prettyLCtx ctx <+> dot <+> prettyTerm 1 t)
prettyTerm p (TLetBox u e1 e2) =
  wrap p 1 $ hsep [ "let"
                  , "box" <> brackets (prettyGId u)
                  , "="
                  , prettyTerm 1 e1
                  , "in"
                  , prettyTerm 1 e2
                  ]
prettyTerm p (TClo u s) = wrap p 1 $ hsep [prettyGId u, "with", prettySubst s]
prettyTerm p (TLet x e1 e2) =
  wrap p 1 $ hsep [ "let"
                  , prettyId x
                  , "="
                  , prettyTerm 1 e1
                  , "in"
                  , prettyTerm 1 e2
                  ]
prettyTerm p (TPrimOp op) = go op
  where
    go (IntEq e1 e2)  = wrap p 4 $ hsep [ prettyTerm 5 e1, "==", prettyTerm 5 e2 ]
    go (IntLe e1 e2)  = wrap p 4 $ hsep [ prettyTerm 5 e1, "<=", prettyTerm 5 e2 ]
    go (IntLt e1 e2)  = wrap p 4 $ hsep [ prettyTerm 5 e1, "<", prettyTerm 5 e2 ]
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

prettySubst :: Subst -> Doc ann
prettySubst s = brackets $ hsep $ punctuate comma $ map go $ reverse s
  where
    go (x,e) = hsep [prettyTerm 1 e, "/", prettyId x]
