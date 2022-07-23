module PrettyPrinter where

import           Prettyprinter
import           Syntax

prettyId :: Id -> Doc ann
prettyId (Id x i) = pretty $ mconcat [x,"_",show i]

prettyGId :: GId -> Doc ann
prettyGId (GId u i) = pretty $ mconcat [u,"_",show i]

arrow :: Doc ann
arrow = "->"

vdash :: Doc ann
vdash = "|-"

typing :: Id -> Type -> Doc ann
typing x t = hsep [prettyId x, colon, prettyType 1 t]

prettyTerm :: Int -> Term -> Doc ann
prettyTerm _ (Var x) = prettyId x
prettyTerm p (Lam x t e)
  | p > 1     = parens doc
  | otherwise = doc
  where
    doc = hsep [ "fn"
               , parens $ typing x t
               , arrow
               , prettyTerm 1 e
               ]
prettyTerm p (App t1 t2)
  | p > 2     = parens doc
  | otherwise = doc
  where
    doc = prettyTerm 2 t1 <+> prettyTerm 3 t2
prettyTerm _ (Box ctx t) = "box" <> brackets (prettyLCtx ctx <+> dot <+> prettyTerm 1 t)
prettyTerm p (LetBox ectx u e1 e2)
  | p > 1     = parens doc
  | otherwise = doc
  where
    doc = hsep [ "let"
               , "box" <> brackets (prettyLECtx ectx <+> dot <+> prettyGId u)
               , "="
               , prettyTerm 1 e1
               , "in"
               , prettyTerm 1 e2
               ]
prettyTerm p (Clo u es)
  | p > 1     = parens doc
  | otherwise = doc
  where
    doc = hsep [prettyGId u, "with", tupled (map (prettyTerm 1) es)]

prettyType :: Int -> Type -> Doc ann
prettyType _ Base = "base"
prettyType p (Arr t1 t2)
  | p > 1     = parens doc
  | otherwise = doc
  where
    doc = hsep [ prettyType 2 t1
               , arrow
               , prettyType 1 t2
               ]
prettyType _ (BoxT ctx t) = brackets $
  hsep [ prettyLCtx ctx
       , vdash
       , prettyType 1 t
       ]

prettyLCtx :: LCtx -> Doc ann
prettyLCtx ctx = hsep $ punctuate comma $ map (uncurry typing) ctx

prettyLECtx :: LECtx -> Doc ann
prettyLECtx ectx = hsep $ punctuate comma $ map prettyId ectx
