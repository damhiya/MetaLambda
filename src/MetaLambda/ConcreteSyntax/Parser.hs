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
  -- types
  base <- E.rule $ do
    tok TBase
    pure Base

  boxt <- E.rule $ brackets $ do
    g <- ctx
    tok TVDash
    t <- typ1
    pure (BoxT g t)

  arr <- E.rule $ do
    t1 <- typ2
    tok TArrow
    t2 <- typ1
    pure (Arr t1 t2)

  typ2 <- E.rule $ base <|> boxt <|> parens typ1
  typ1 <- E.rule $ typ2 <|> arr

  -- contexts
  ctx <- E.rule $ sepBy' (tok TComma) $ do
    x <- ident
    tok TColon
    t <- typ1
    pure (x, t)

  ectx <- E.rule $ sepBy' (tok TComma) ident

  -- terms
  var <- E.rule $ do
    x <- ident
    pure (Var x)

  box <- E.rule $ tok TBox *> (brackets $ do
    g <- ctx
    tok TDot
    e <- term1
    pure (Box g e))

  lam <- E.rule $ do
    tok TFn
    xt <- parens $ do
      x <- ident
      tok TColon
      t <- typ1
      pure (x,t)
    tok TArrow
    e <- term1
    pure (uncurry Lam xt e)

  letbox <- E.rule $ do
    tok TLet
    tok TBox
    gu <- brackets $ do
      g <- ectx
      tok TDot
      u <- gident
      pure (g,u)
    tok TEqual
    e1 <- term1
    tok TIn
    e2 <- term1
    pure (uncurry LetBox gu e1 e2)

  clo <- E.rule $ do
    u <- gident
    tok TWith
    es <- parens $ sepBy' (tok TComma) term1
    pure (Clo u es)

  app <- E.rule $ do
    e1 <- term2
    e2 <- term3
    pure (App e1 e2)

  term3 <- E.rule $ var <|> box <|> parens term1
  term2 <- E.rule $ term3 <|> app
  term1 <- E.rule $ term2 <|> lam <|> letbox <|> clo

  pure term1
  where
    tok t = E.satisfy (\(Token _ t') -> t' == t) $> ()

    ident = E.terminal $ \case
      Token _ (TIdent x) -> Just (Id x 0)
      _                  -> Nothing

    gident = E.terminal $ \case
      Token _ (TIdent x) -> Just (GId x 0)
      _                  -> Nothing

    sepBy d p = pure [] <|> liftA2 (:) p (many (d *> p))
    sepBy' d p = reverse <$> sepBy d p
    parens p = tok TParL *> p <* tok TParR
    brackets p = tok TBrkL *> p <* tok TBrkR

parser :: MonadError ParseError m => [Token] -> m Term
parser ts =
  case E.fullParses (E.parser grammar) ts of
    ([e], r) | null (E.unconsumed r) -> pure e
    err                              -> throwError err
