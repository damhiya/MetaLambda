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
typing x t = hsep [prettyId x, colon, prettyType t]

prettyTerm :: Term -> Doc ann
prettyTerm (Var x) = prettyId x
prettyTerm (Lam x t e) = hsep [ "fn"
                              , parens $ typing x t
                              , arrow
                              , prettyTerm e
                              ]
prettyTerm (App t1 t2) = parens (prettyTerm t1 <+> prettyTerm t2)
prettyTerm (Box ctx t) = "box" <> brackets (prettyLCtx ctx <+> dot <+> prettyTerm t)
prettyTerm (LetBox ectx u e1 e2) = hsep [ "let"
                                        , "box" <> brackets (prettyLECtx ectx <+> dot <+> prettyGId u)
                                        , "="
                                        , prettyTerm e1
                                        , "in"
                                        , prettyTerm e2
                                        ]
prettyTerm (Clo u es) = hsep [prettyGId u, "with", tupled (map prettyTerm es)]


prettyType :: Type -> Doc ann
prettyType Base = "base"
prettyType (Arr t1 t2) = parens $ hsep [ prettyType t1
                                       , arrow
                                       , prettyType t2
                                       ]
prettyType (BoxT ctx t) = brackets $
  hsep [ prettyLCtx ctx
       , vdash
       , prettyType t
       ]

prettyLCtx :: LCtx -> Doc ann
prettyLCtx ctx = hsep $ punctuate comma $ map (uncurry typing) ctx

prettyLECtx :: LECtx -> Doc ann
prettyLECtx ectx = hsep $ punctuate comma $ map prettyId ectx
