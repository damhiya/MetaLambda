{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}

module Parser.Parser where

import Control.Applicative
import Data.Functor
import qualified Text.Earley as E

import Syntax
import Parser.Syntax

grammar :: E.Grammar r (E.Prod r Token Token Term)
grammar = mdo
  -- types
  base <- E.rule $ do
    tok TBase
    pure Base
    
  boxt <- E.rule $ brackets $ do
    g <- ctx
    tok TVDash
    t <- typ
    pure (BoxT g t)
  
  atyp <- E.rule $ base <|> boxt <|> parens typ

  arr <- E.rule $ do
    t1 <- atyp
    tok TArrow
    t2 <- typ
    pure (Arr t1 t2)
  
  typ <- E.rule $ atyp <|> arr
    
  -- contexts
  ctx <- E.rule $ sepBy' (tok TComma) $ do
    x <- ident
    tok TColon
    t <- typ
    pure (x, t)

  ectx <- E.rule $ sepBy' (tok TComma) ident
  
  -- terms
  var <- E.rule $ do
    x <- ident
    pure (Var x)
    
  box <- E.rule $ tok TBox *> brackets do
      g <- ctx
      tok TDot
      e <- term
      pure (Box g e)
    
  lam <- E.rule $ do
    tok TFn
    xt <- parens $ do
      x <- ident
      tok TColon
      t <- typ
      pure (x,t)
    tok TArrow
    e <- term
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
    e1 <- term
    tok TIn
    e2 <- term
    pure (uncurry LetBox gu e1 e2)
    
  clo <- E.rule $ do
    u <- gident
    tok TWith
    es <- parens $ sepBy' (tok TComma) term
    pure (Clo u es)
  
  aterm <- E.rule $ var <|> box <|> parens term

  app <- E.rule $ do
    e1 <- term
    e2 <- aterm
    pure (App e1 e2)
    
  term <- E.rule $ aterm <|> lam <|> letbox <|> clo <|> app
  
  pure term
  where
    tok t = E.satisfy (\(Token _ t') -> t' == t) $> ()

    ident = E.terminal $ \case
      Token _ (TIdent x) -> Just (Id x 0)
      _ -> Nothing

    gident = E.terminal $ \case
      Token _ (TIdent x) -> Just (GId x 0)
      _ -> Nothing
    
    sepBy d p = pure [] <|> liftA2 (:) p (many (d *> p))
    sepBy' d p = reverse <$> sepBy d p
    parens p = tok TParL *> p <* tok TParR
    brackets p = tok TBrkL *> p <* tok TBrkR

parser :: [Token] -> ([Term], E.Report Token [Token])
parser = E.fullParses (E.parser grammar)
