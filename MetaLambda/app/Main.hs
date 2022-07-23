module Main where

import           Control.Monad.Except
import           Data.Bifunctor
import qualified Data.Text                as T
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
  getLineRepl :: String -> m String
  putStrRepl :: String -> m ()
  putStrLnRepl :: String -> m ()

printRepl :: (MonadRepl m, Show a) => a -> m ()
printRepl = putStrLnRepl . show

liftReplError :: MonadError ReplError m => (e -> ReplError) -> Either e a -> m a
liftReplError f = liftEither . first f

instance MonadRepl (ExceptT ReplError (InputT IO)) where
  getLineRepl s = ExceptT (with ErrInput <$> getInputLine s)
  putStrRepl s = ExceptT (Right <$> outputStr s)
  putStrLnRepl s = ExceptT (Right <$> outputStrLn s)

body :: MonadRepl m => m ()
body = do
  s <- getLineRepl ">> "
  ts <- liftReplError ErrLex $ tokenize "stdin" (T.pack s)
  e  <- liftReplError ErrParse $ parser ts
  printRepl (prettyTerm 1 e)
  t  <- liftReplError ErrType $ inferType [] [] e
  printRepl (prettyType 1 t)
  printRepl (prettyTerm 1 (eval e))

loop :: InputT IO ()
loop = runExceptT body >>= \case
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

main :: IO ()
main = runInputT defaultSettings loop
