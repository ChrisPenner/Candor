{-# language ViewPatterns #-}
module REPL where

import RIO
import Eval
import Parse
import TypeInference
import Data.Text.IO as TIO
import Data.Text
import AST

repl :: IO ()
repl = do
  ln <- strip <$> TIO.getLine
  putStrLn . pack $ 
    case stripPrefix ":t" ln of
      Just (strip -> rest) -> 
        case parse (unpack rest) of
          Right ast -> either show pretty $ inferType ast
          Left err -> show err
      Nothing -> either show pretty (parse (unpack ln) >>= eval)
  repl
