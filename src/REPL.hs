{-# language ViewPatterns #-}
module REPL where

import RIO
import Eval
import Parse
import TypeInference
import System.IO (print)
import Data.Text.IO as TIO
import Data.Text
import Data.Bifunctor as Bi (first)

repl :: IO ()
repl = do
  ln <- strip <$> TIO.getLine
  case stripPrefix ":t" ln of
    Just (strip -> rest) -> print (parse (unpack rest) >>= Bi.first show . inferType)
    Nothing -> print (parse (unpack ln) >>= eval)
  repl
