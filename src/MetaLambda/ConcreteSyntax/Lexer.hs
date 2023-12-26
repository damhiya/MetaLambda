module MetaLambda.ConcreteSyntax.Lexer where

import           Control.Applicative
import           Control.Monad.Except
import           Data.Char
import           Data.Functor
import           Data.Text
import           Data.Void
import qualified MetaLambda.ConcreteSyntax.Token as T
import qualified Text.Megaparsec                 as P
import qualified Text.Megaparsec.Char            as P

type PM = P.Parsec Void Text
type LexError = P.ParseErrorBundle Text Void

token :: PM T.Token
token = do
  pos <- P.getSourcePos
  tok <- P.choice
    [ P.string "."  $> T.Dot
    , P.string ","  $> T.Comma
    , P.string "::" $> T.Cons
    , P.string ":"  $> T.Colon
    , P.string "==" $> T.Eq
    , P.string "<=" $> T.Le
    , P.string "<"  $> T.Lt
    , P.string "="  $> T.Equal
    , P.string "->" $> T.Arrow
    , P.string "|-" $> T.RTack
    , P.string "|"  $> T.Bar
    , P.string "["  $> T.BrkL
    , P.string "]"  $> T.BrkR
    , P.string "("  $> T.ParL
    , P.string ")"  $> T.ParR
    , P.string "+"  $> T.Plus
    , P.string "-"  $> T.Minus
    , P.string "*"  $> T.Prod
    , P.string "/"  $> T.Div
    , P.string "%"  $> T.Mod
    , P.string "^"  $> T.Pow
    , do
        x <- headChar
        xs <- many tailChar
        pure $ case x:xs of
          "fn"     -> T.Fn
          "fix"    -> T.Fix
          "box"    -> T.Box
          "let"    -> T.Let
          "in"     -> T.In
          "match"  -> T.Match
          "with"   -> T.With
          "end"    -> T.End
          "if"     -> T.If
          "then"   -> T.Then
          "else"   -> T.Else
          "base"   -> T.Base
          "bool"   -> T.Bool
          "int"    -> T.Int
          "list"   -> T.List
          "true"   -> T.True
          "false"  -> T.False
          "of"     -> T.Of
          "inject" -> T.Inject
          ident    -> T.Ident ident
    , do
        ds <- some P.digitChar
        let n = number 0 ds
        pure (T.Num n)
    ]
  P.space
  pure (T.Token pos tok)
  where
    headChar = P.letterChar <|> P.char '_'
    tailChar = P.alphaNumChar <|> P.char '_'
    number :: Integer -> String -> Integer
    number a []     = a
    number a (d:ds) = number (a*10 + n) ds
      where
        n = fromIntegral (ord d - ord '0')

tokens :: PM [T.Token]
tokens = P.space *> many token <* P.eof

tokenize :: MonadError LexError m => String -> Text -> m [T.Token]
tokenize f s = liftEither (P.parse tokens f s)
