{-# language OverloadedStrings #-}
{-# language GeneralizedNewtypeDeriving #-}
module Main where

import RIO
import System.Environment
import System.IO
import Parse
import Eval
import AST
import CLI
import REPL
import Options.Applicative (execParser)

newtype ParseError = ParseError String
instance Show ParseError where
  show (ParseError err) = "Parse Error: " ++ err

newtype EvalError = EvalError String
instance Show EvalError where
  show (EvalError err) = "Eval Error: " ++ err

newtype ArgError = ArgError String
instance Show ArgError where
  show (ArgError err) = err

instance Exception ParseError
instance Exception EvalError
instance Exception ArgError

main :: IO ()
main = do
  Options runRepl <- execParser opts
  if runRepl 
     then repl 
     else compiler


compiler :: IO ()
compiler = flip catchAny print $ do
  args <- getArgs
  file <- case args of
                [fileName] -> readFile fileName
                _ -> throwIO $ ArgError "usage: candor <filename>"
  ast <- either (throwIO . ParseError) return (parse file)
  result <- either (throwIO . EvalError) return (eval ast)
  putStrLn . pretty $ result
