{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RecursiveDo   #-}

module MetaLambda.ConcreteSyntax.Parser where

import           Control.Applicative
import           Control.Monad.Except
import           Data.Functor
import qualified Text.Earley                     as E

import qualified MetaLambda.ConcreteSyntax.Token as T
import           MetaLambda.Syntax

type ParseError = ([Term], E.Report T.Token [T.Token])

grammar :: E.Grammar r (E.Prod r T.Token T.Token Term)
grammar = mdo
  {- types -}
  base <- E.rule $ do
    tok T.Base
    pure Base

  bool <- E.rule $ do
    tok T.Bool
    pure Bool

  int <- E.rule $ do
    tok T.Int
    pure Int

  prod <- E.rule $ do
    t1 <- typ2
    tok T.Prod
    t2 <- typ3
    pure (Prod t1 t2)

  list <- E.rule $ do
    tok T.List
    t <- typ3
    pure (List t)

  boxt <- E.rule $ brackets $ do
    g <- ctx
    tok T.RTack
    t <- typ1
    pure (BoxT g t)

  arr <- E.rule $ do
    t1 <- typ2
    tok T.Arrow
    t2 <- typ1
    pure (Arr t1 t2)

  typ3 <- E.rule $ base <|> bool <|> int <|> list <|> boxt <|> parens typ1
  typ2 <- E.rule $ typ3 <|> prod
  typ1 <- E.rule $ typ2 <|> arr

  {- contexts -}
  ctx <- E.rule $ sepBy' (tok T.Comma) $ do
    x <- ident
    tok T.Colon
    t <- typ1
    pure (x, t)

  {- substitutions -}
  subst <- E.rule $ brackets $ sepBy' (tok T.Comma) $ do
    e <- term1
    tok T.Div
    x <- ident
    pure (x,e)

  {- terms -}
  var <- E.rule $ do
    x <- ident
    pure (TVar x)

  -- bool
  true <- E.rule $ do
    tok T.True
    pure TTrue

  false <- E.rule $ do
    tok T.False
    pure TFalse

  boolMatch <- E.rule $ do
    tok T.Match
    e <- term2
    tok T.With
    tok T.Bar
    tok T.True
    tok T.Arrow
    e1 <- term1
    tok T.Bar
    tok T.False
    tok T.Arrow
    e2 <- term1
    tok T.End
    pure (TBoolMatch e e1 e2)

  ifThenElse <- E.rule $ do
    tok T.If
    e <- term2
    tok T.Then
    e1 <- term1
    tok T.Else
    e2 <- term1
    pure (TBoolMatch e e1 e2)

  -- integer
  intLit <- E.rule $ do
    n <- number
    pure (TInt n)

  -- product
  pair <- E.rule $ parens $ do
    e1 <- term1
    tok T.Comma
    e2 <- term1
    pure (TPair e1 e2)

  prodMatch <- E.rule $ do
    tok T.Match
    e <- term2
    tok T.With
    tok T.Bar
    tok T.ParL
    x <- ident
    tok T.Comma
    y <- ident
    tok T.ParR
    tok T.Arrow
    e1 <- term1
    tok T.End
    pure (TProdMatch e x y e1)

  -- list
  nil <- E.rule $ do
    brackets $ pure ()
    tok T.Of
    t <- typ3
    pure (TNil t)

  cons <- E.rule $ do
    e1 <- term6
    tok T.Cons
    e2 <- term5
    pure (TCons e1 e2)

  listMatch <- E.rule $ do
    tok T.Match
    e <- term2
    tok T.With
    tok T.Bar
    brackets $ pure ()
    tok T.Arrow
    e1 <- term1
    tok T.Bar
    x <- ident
    tok T.Cons
    xs <- ident
    tok T.Arrow
    e2 <- term1
    tok T.End
    pure (TListMatch e e1 x xs e2)

  -- function
  lam <- E.rule $ do
    tok T.Fn
    tok T.ParL
    x <- ident
    tok T.Colon
    t <- typ1
    tok T.ParR
    tok T.Arrow
    e <- term1
    pure (TLam x t e)

  fix <- E.rule $ do
    tok T.Fix
    tok T.ParL
    f <- ident
    tok T.Colon
    t1 <- typ2
    tok T.Arrow
    t2 <- typ1
    tok T.ParR
    x <- ident
    tok T.Arrow
    e <- term1
    pure (TFix t1 t2 f x e)

  app <- E.rule $ do
    e1 <- term9
    e2 <- term10
    pure (TApp e1 e2)

  -- box
  box <- E.rule $ do
    tok T.Box
    tok T.BrkL
    g <- ctx
    tok T.Dot
    e <- term1
    tok T.BrkR
    pure (TBox g e)

  letbox <- E.rule $ do
    tok T.Let
    tok T.Box
    tok T.BrkL
    u <- gident
    tok T.BrkR
    tok T.Equal
    e1 <- term1
    tok T.In
    e2 <- term1
    pure (TLetBox u e1 e2)

  clo <- E.rule $ do
    u <- gident
    tok T.With
    s <- subst
    pure (TClo u s)

  -- others
  letin <- E.rule $ do
    tok T.Let
    x <- ident
    tok T.Equal
    e1 <- term1
    tok T.In
    e2 <- term1
    pure (TLet x e1 e2)

  intEq <- E.rule $ do
    e1 <- term5
    tok T.Eq
    e2 <- term5
    pure (TPrimOp (IntEq e1 e2))

  intLe <- E.rule $ do
    e1 <- term5
    tok T.Le
    e2 <- term5
    pure (TPrimOp (IntLe e1 e2))

  intLt <- E.rule $ do
    e1 <- term5
    tok T.Lt
    e2 <- term5
    pure (TPrimOp (IntLt e1 e2))

  intAdd <- E.rule $ do
    e1 <- term6
    tok T.Plus
    e2 <- term7
    pure (TPrimOp (IntAdd e1 e2))

  intSub <- E.rule $ do
    e1 <- term6
    tok T.Minus
    e2 <- term7
    pure (TPrimOp (IntSub e1 e2))

  intMul <- E.rule $ do
    e1 <- term7
    tok T.Prod
    e2 <- term8
    pure (TPrimOp (IntMul e1 e2))

  intDiv <- E.rule $ do
    e1 <- term7
    tok T.Div
    e2 <- term8
    pure (TPrimOp (IntDiv e1 e2))

  intMod <- E.rule $ do
    e1 <- term7
    tok T.Mod
    e2 <- term8
    pure (TPrimOp (IntMod e1 e2))

  intPow <- E.rule $ do
    e1 <- term9
    tok T.Pow
    e2 <- term8
    pure (TPrimOp (IntPow e1 e2))

  inject <- E.rule $ do
    t1 <- tok T.Inject
    t <- term10
    pure (TPrimOp (Inject t))

  term11 <- E.rule $ var <|> true <|> false <|> boolMatch <|> intLit
                 <|> pair <|> prodMatch <|> listMatch <|> box <|> parens term1
  term10 <- E.rule $ term11 <|> clo <|> nil
  term9  <- E.rule $ term10 <|> app <|> inject
  term8  <- E.rule $ term9 <|> intPow
  term7  <- E.rule $ term8 <|> intMul <|> intDiv <|> intMod
  term6  <- E.rule $ term7 <|> intAdd <|> intSub
  term5  <- E.rule $ term6 <|> cons
  term4  <- E.rule $ term5 <|> intEq <|> intLe <|> intLt
  let term2 = term4
  term1  <- E.rule $ term2 <|> lam <|> fix <|> letbox <|> letin <|> ifThenElse

  pure term1
  where
    tok t = E.satisfy (\(T.Token _ t') -> t' == t) $> ()

    ident = E.terminal $ \case
      T.Token _ (T.Ident x) -> Just (Id x 0)
      _                  -> Nothing

    gident = E.terminal $ \case
      T.Token _ (T.Ident x) -> Just (GId x 0)
      _                  -> Nothing

    number = E.terminal $ \case
      T.Token _ (T.Num n)   -> Just n
      _                   -> Nothing

    sepBy d p = pure [] <|> liftA2 (:) p (many (d *> p))
    sepBy' d p = reverse <$> sepBy d p
    parens p = tok T.ParL *> p <* tok T.ParR
    brackets p = tok T.BrkL *> p <* tok T.BrkR

parser :: MonadError ParseError m => [T.Token] -> m Term
parser ts =
  case E.fullParses (E.parser grammar) ts of
    ([e], r) | null (E.unconsumed r) -> pure e
    err                              -> throwError err
