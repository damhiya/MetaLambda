{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RecursiveDo   #-}

module MetaLambda.ConcreteSyntax.Parser where

import           Control.Applicative
import           Control.Monad.Except
import           Data.Functor
import qualified Text.Earley          as E

import           MetaLambda.ConcreteSyntax.Token
import           MetaLambda.Syntax

type ParseError = ([Term], E.Report Token [Token])

grammar :: E.Grammar r (E.Prod r Token Token Term)
grammar = mdo
  {- types -}
  base <- E.rule $ do
    tok TBase
    pure Base

  bool <- E.rule $ do
    tok TBool
    pure Bool

  int <- E.rule $ do
    tok TInt
    pure Int

  prod <- E.rule $ do
    t1 <- typ2
    tok TProd
    t2 <- typ3
    pure (Prod t1 t2)

  list <- E.rule $ do
    tok TList
    t <- typ3
    pure (List t)

  boxt <- E.rule $ brackets $ do
    g <- ctx
    tok TRTack
    t <- typ1
    pure (BoxT g t)

  arr <- E.rule $ do
    t1 <- typ2
    tok TArrow
    t2 <- typ1
    pure (Arr t1 t2)

  typ3 <- E.rule $ base <|> bool <|> int <|> list <|> boxt <|> parens typ1
  typ2 <- E.rule $ typ3 <|> prod
  typ1 <- E.rule $ typ2 <|> arr

  {- contexts -}
  ctx <- E.rule $ sepBy' (tok TComma) $ do
    x <- ident
    tok TColon
    t <- typ1
    pure (x, t)

  ectx <- E.rule $ sepBy' (tok TComma) ident

  {- terms -}
  var <- E.rule $ do
    x <- ident
    pure (Var x)

  -- bool
  true <- E.rule $ do
    tok TTrue
    pure BTrue

  false <- E.rule $ do
    tok TFalse
    pure BFalse

  boolMatch <- E.rule $ do
    tok TMatch
    e <- term2
    tok TWith
    tok TBar
    tok TTrue
    tok TArrow
    e1 <- term1
    tok TBar
    tok TFalse
    tok TArrow
    e2 <- term1
    tok TEnd
    pure (BoolMatch e e1 e2)

  -- integer
  intLit <- E.rule $ do
    n <- number
    pure (IntLit n)

  -- product
  pair <- E.rule $ parens $ do
    e1 <- term1
    tok TComma
    e2 <- term1
    pure (Pair e1 e2)

  prodMatch <- E.rule $ do
    tok TMatch
    e <- term2
    tok TWith
    tok TBar
    tok TParL
    x <- ident
    tok TComma
    y <- ident
    tok TParR
    tok TArrow
    e1 <- term1
    tok TEnd
    pure (ProdMatch e x y e1)

  -- list
  nil <- E.rule $ do
    brackets $ pure ()
    tok TOf
    t <- typ3
    pure (Nil t)

  cons <- E.rule $ do
    e1 <- term6
    tok TCons
    e2 <- term5
    pure (Cons e1 e2)

  listMatch <- E.rule $ do
    tok TMatch
    e <- term2
    tok TWith
    tok TBar
    brackets $ pure ()
    tok TArrow
    e1 <- term1
    tok TBar
    x <- ident
    tok TCons
    xs <- ident
    tok TArrow
    e2 <- term1
    tok TEnd
    pure (ListMatch e e1 x xs e2)

  -- function
  lam <- E.rule $ do
    tok TFn
    tok TParL
    x <- ident
    tok TColon
    t <- typ1
    tok TParR
    tok TArrow
    e <- term1
    pure (Lam x t e)

  fix <- E.rule $ do
    tok TFix
    tok TParL
    f <- ident
    tok TColon
    t1 <- typ2
    tok TArrow
    t2 <- typ1
    tok TParR
    x <- ident
    tok TArrow
    e <- term1
    pure (Fix t1 t2 f x e)

  app <- E.rule $ do
    e1 <- term9
    e2 <- term10
    pure (App e1 e2)

  -- box
  box <- E.rule $ do
    tok TBox
    tok TBrkL
    g <- ctx
    tok TDot
    e <- term1
    tok TBrkR
    pure (Box g e)

  letbox <- E.rule $ do
    tok TLet
    tok TBox
    tok TBrkL
    g <- ectx
    tok TDot
    u <- gident
    tok TBrkR
    tok TEqual
    e1 <- term1
    tok TIn
    e2 <- term1
    pure (LetBox g u e1 e2)

  -- others
  clo <- E.rule $ do
    u <- gident
    tok TWith
    es <- parens $ sepBy' (tok TComma) term1
    pure (Clo u es)

  letin <- E.rule $ do
    tok TLet
    x <- ident
    tok TEqual
    e1 <- term1
    tok TIn
    e2 <- term1
    pure (Let x e1 e2)

  intEq <- E.rule $ do
    e1 <- term5
    tok TEq
    e2 <- term5
    pure (PrimOp (IntEq e1 e2))

  intLe <- E.rule $ do
    e1 <- term5
    tok TLe
    e2 <- term5
    pure (PrimOp (IntLe e1 e2))

  intAdd <- E.rule $ do
    e1 <- term6
    tok TPlus
    e2 <- term7
    pure (PrimOp (IntAdd e1 e2))

  intSub <- E.rule $ do
    e1 <- term6
    tok TMinus
    e2 <- term7
    pure (PrimOp (IntSub e1 e2))

  intMul <- E.rule $ do
    e1 <- term7
    tok TProd
    e2 <- term8
    pure (PrimOp (IntMul e1 e2))

  intDiv <- E.rule $ do
    e1 <- term7
    tok TDiv
    e2 <- term8
    pure (PrimOp (IntDiv e1 e2))

  intMod <- E.rule $ do
    e1 <- term7
    tok TMod
    e2 <- term8
    pure (PrimOp (IntMod e1 e2))

  intPow <- E.rule $ do
    e1 <- term9
    tok TPow
    e2 <- term8
    pure (PrimOp (IntPow e1 e2))

  inject <- E.rule $ do
    t1 <- tok TInject
    t <- term10
    pure (PrimOp (Inject t))

  term11 <- E.rule $ var <|> true <|> false <|> boolMatch <|> intLit
                 <|> pair <|> prodMatch <|> listMatch <|> box <|> parens term1
  term10 <- E.rule $ term11 <|> clo <|> nil
  term9  <- E.rule $ term10 <|> app <|> inject
  term8  <- E.rule $ term9 <|> intPow
  term7  <- E.rule $ term8 <|> intMul <|> intDiv <|> intMod
  term6  <- E.rule $ term7 <|> intAdd <|> intSub
  term5  <- E.rule $ term6 <|> cons
  term4  <- E.rule $ term5 <|> intEq <|> intLe
  let term2 = term4
  term1  <- E.rule $ term2 <|> lam <|> fix <|> letbox <|> letin

  pure term1
  where
    tok t = E.satisfy (\(Token _ t') -> t' == t) $> ()

    ident = E.terminal $ \case
      Token _ (TIdent x) -> Just (Id x 0)
      _                  -> Nothing

    gident = E.terminal $ \case
      Token _ (TIdent x) -> Just (GId x 0)
      _                  -> Nothing

    number = E.terminal $ \case
      Token _ (TIntLit n) -> Just n
      _                   -> Nothing

    sepBy d p = pure [] <|> liftA2 (:) p (many (d *> p))
    sepBy' d p = reverse <$> sepBy d p
    parens p = tok TParL *> p <* tok TParR
    brackets p = tok TBrkL *> p <* tok TBrkR

parser :: MonadError ParseError m => [Token] -> m Term
parser ts =
  case E.fullParses (E.parser grammar) ts of
    ([e], r) | null (E.unconsumed r) -> pure e
    err                              -> throwError err
