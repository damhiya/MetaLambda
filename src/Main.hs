module Main where

import           Control.Monad.IO.Class
import qualified Data.Text                as T
import qualified Data.Text.IO             as T
import           System.Console.Haskeline
import           System.Exit
import           System.IO
import           Text.Megaparsec          as P

import           Parser.Lexer
import           Parser.Parser
import           PrettyPrinter
import           Reduction.Evaluation
import qualified Text.Earley.Parser       as E
import           Typing

main :: IO ()
main = runInputT defaultSettings loop
  where
    lexing s = case tokenize "stdin" s of
      Left b -> do
        putStrLn "lexing error"
        putStr (P.errorBundlePretty b)
        exitFailure
      Right e -> pure e
    parsing ts = case parser ts of
      ([e], r) | null (E.unconsumed r) -> pure e
      (es, r) -> do
        putStrLn "parsing error"
        print es
        print r
        exitFailure
    loop :: InputT IO ()
    loop = getInputLine ">> " >>= \case
      Nothing -> liftIO $ putStrLn "bye"
      Just s -> do
        ts <- liftIO $ lexing (T.pack s)
        e <- liftIO $ parsing ts
        liftIO $ print (prettyTerm e)
        case inferType [] [] e of
          Left e -> do
            liftIO $ putStrLn "Type error"
            liftIO $ print e
            loop
          Right t -> do
            liftIO $ print (prettyType t)
            liftIO $ print (prettyTerm (eval e))
            loop

