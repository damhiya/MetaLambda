module Main where

import System.Exit
import System.IO
import qualified Data.Text.IO as T
import Text.Megaparsec as P

import Parser.Lexer
import Parser.Parser
import qualified Text.Earley.Parser as E
import Typing
import Reduction.Evaluation

main :: IO ()
main = do
  putStr ">> "
  hFlush stdout
  s <- T.getLine
  ts <- lexing s
  e <- parsing ts
  print e
  print (inferType [] [] e)
  print (eval e)
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
