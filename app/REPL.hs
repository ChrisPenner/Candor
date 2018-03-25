{-# language ViewPatterns #-}
module REPL where

import RIO
import Eval
import Parse
import TypeInference
import Data.Text
import AST
import System.Console.Haskeline

repl :: IO ()
repl = runInputT defaultSettings loop
   where
       loop :: InputT IO ()
       loop = do
           minput <- getInputLine "> "
           case minput of
               Nothing -> return ()
               Just "quit" -> return ()
               Just input -> do outputStrLn $ evalString input
                                loop

evalString :: String -> String
evalString ln =
  case stripPrefix ":t" (pack ln) of
    Just (strip -> rest) ->
      case parse (unpack rest) of
        Right ast -> either show pretty $ inferType ast
        Left err -> err
    Nothing -> either show pretty (parse ln >>= eval)
