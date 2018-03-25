{-# language ViewPatterns #-}
module REPL where

import RIO
import Eval
import Parse
import TypeInference
import System.IO (print)
import Data.Text.IO as TIO
import Data.Text
import Types

repl :: IO ()
repl = do
  ln <- strip <$> TIO.getLine
  case stripPrefix ":t" ln of
    Just (strip -> rest) -> putStrLn . pack $
      case parse (unpack rest) of
        Right ast -> either show pretty $ inferType ast
        Left err -> show err
    Nothing -> print (parse (unpack ln) >>= eval)
  repl
