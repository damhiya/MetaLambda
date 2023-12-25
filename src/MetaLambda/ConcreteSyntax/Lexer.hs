module MetaLambda.ConcreteSyntax.Lexer where

import           Control.Applicative
import           Control.Monad.Except
import           Data.Functor
import           Data.Text
import           Data.Char
import           Data.Void
import           MetaLambda.ConcreteSyntax.Token
import qualified Text.Megaparsec      as P
import qualified Text.Megaparsec.Char as P

type PM = P.Parsec Void Text
type LexError = P.ParseErrorBundle Text Void

token :: PM Token
token = do
  pos <- P.getSourcePos
  tok <- P.choice
    [ P.string "."  $> TDot
    , P.string ","  $> TComma
    , P.string "::" $> TCons
    , P.string ":"  $> TColon
    , P.string "==" $> TEq
    , P.string "<=" $> TLe
    , P.string "<"  $> TLt
    , P.string "="  $> TEqual
    , P.string "->" $> TArrow
    , P.string "|-" $> TRTack
    , P.string "|"  $> TBar
    , P.string "["  $> TBrkL
    , P.string "]"  $> TBrkR
    , P.string "("  $> TParL
    , P.string ")"  $> TParR
    , P.string "+"  $> TPlus
    , P.string "-"  $> TMinus
    , P.string "*"  $> TProd
    , P.string "/"  $> TDiv
    , P.string "%"  $> TMod
    , P.string "^"  $> TPow
    , do
        x <- headChar
        xs <- many tailChar
        pure $ case x:xs of
          "fn"   -> TFn
          "fix"  -> TFix
          "box"  -> TBox
          "let"  -> TLet
          "in"   -> TIn
          "match"-> TMatch
          "with" -> TWith
          "end"  -> TEnd
          "base" -> TBase
          "bool" -> TBool
          "int"  -> TInt
          "list" -> TList
          "true" -> TTrue
          "false"-> TFalse
          "of"   -> TOf
          "inject"-> TInject
          ident  -> TIdent ident
    , do
        ds <- some P.digitChar
        let n = number 0 ds
        pure (TIntLit n)
    ]
  P.space
  pure (Token pos tok)
  where
    headChar = P.letterChar <|> P.char '_'
    tailChar = P.alphaNumChar <|> P.char '_'
    number :: Integer -> String -> Integer
    number a []     = a
    number a (d:ds) = number (a*10 + n) ds
      where
        n = fromIntegral (ord d - ord '0')

tokens :: PM [Token]
tokens = P.space *> many token <* P.eof

tokenize :: MonadError LexError m => String -> Text -> m [Token]
tokenize f s = liftEither (P.parse tokens f s)
