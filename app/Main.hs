{-# language OverloadedStrings #-}
{-# language GeneralizedNewtypeDeriving #-}
module Main where

import RIO
import System.IO
import Parse
import Eval
import AST
import CLI
import REPL
import Options.Applicative (execParser)
import qualified Data.Text as Text

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
  Options runRepl srcFile <- execParser opts
  if runRepl
     then repl
     else compiler srcFile


compiler :: FilePath -> IO ()
compiler srcFile = flip catchAny print $ do
  fileContents <- readFileUtf8 srcFile
  ast <- either (throwIO . ParseError) return (parse $ Text.unpack fileContents)
  result <- either (throwIO . EvalError) return (eval ast)
  putStrLn . pretty $ result
