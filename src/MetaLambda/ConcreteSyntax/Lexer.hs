module MetaLambda.ConcreteSyntax.Lexer where

import           Control.Applicative
import           Control.Monad.Except
import           Data.Functor
import           Data.Text
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
    , P.string ":"  $> TColon
    , P.string "="  $> TEqual
    , P.string "->" $> TArrow
    , P.string "|-" $> TVDash
    , P.string "["  $> TBrkL
    , P.string "]"  $> TBrkR
    , P.string "("  $> TParL
    , P.string ")"  $> TParR
    , do
        x <- headChar
        xs <- many tailChar
        pure $ case x:xs of
          "fn"   -> TFn
          "box"  -> TBox
          "let"  -> TLet
          "in"   -> TIn
          "with" -> TWith
          "base" -> TBase
          ident  -> TIdent ident
    ]
  P.space
  pure (Token pos tok)
  where
    headChar = P.letterChar <|> P.char '_'
    tailChar = P.alphaNumChar <|> P.char '_'

tokens :: PM [Token]
tokens = P.space *> many token <* P.eof

tokenize :: MonadError LexError m => String -> Text -> m [Token]
tokenize f s = liftEither (P.parse tokens f s)
