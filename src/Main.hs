module Main where

import           Control.Monad.Except
import           Control.Monad.IO.Class
import           Data.Bifunctor
import qualified Data.Text                as T
import qualified Data.Text.IO             as T
import           System.Console.Haskeline
import qualified Text.Megaparsec          as P

import           Parser.Lexer
import           Parser.Parser
import           PrettyPrinter
import           Reduction.Evaluation
import           Typing
import           Util

data ReplError
  = ErrInput
  | ErrLex LexError
  | ErrParse ParseError
  | ErrType TypeError

class (Monad m, MonadError ReplError m) => MonadRepl m where
  readRepl :: String -> m String
  putStrRepl :: String -> m ()
  putStrLnRepl :: String -> m ()

printRepl :: (MonadRepl m, Show a) => a -> m ()
printRepl = putStrLnRepl . show

liftReplError :: MonadError ReplError m => (e -> ReplError) -> Either e a -> m a
liftReplError f = liftEither . first f

instance MonadRepl (ExceptT ReplError (InputT IO)) where
  readRepl s = ExceptT (with ErrInput <$> getInputLine s)
  putStrRepl s = ExceptT (Right <$> outputStr s)
  putStrLnRepl s = ExceptT (Right <$> outputStrLn s)

body :: MonadRepl m => m ()
body = do
  s <- readRepl ">> "
  ts <- liftReplError ErrLex $ tokenize "stdin" (T.pack s)
  e  <- liftReplError ErrParse $ parser ts
  printRepl (prettyTerm e)
  t  <- liftReplError ErrType $ inferType [] [] e
  printRepl (prettyType t)
  printRepl (prettyTerm (eval e))

loop :: InputT IO ()
loop = body' >>= \case
  Left ErrInput -> outputStrLn "bye."
  Left (ErrLex e) -> do
    outputStrLn "lex error"
    outputStr (P.errorBundlePretty e)
    loop
  Left (ErrParse (es, r)) -> do
    outputStrLn "parse error"
    outputStrLn (show es)
    outputStrLn (show r)
    loop
  Left (ErrType e) -> do
    outputStrLn "type error"
    outputStrLn (show e)
    loop
  Right () -> loop
  where
    body' = runExceptT body

main :: IO ()
main = runInputT defaultSettings loop
