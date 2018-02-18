module Primitives where

import AST

import RIO
import qualified Data.Map as M

primitives :: Bindings
primitives = M.fromList . fmap embed $
  [ ("+", TFunc [TNumber, TNumber, TNumber])
  , ("-", TFunc [TNumber, TNumber, TNumber])
  , ("*", TFunc [TNumber, TNumber, TNumber])
  , ("=", TFunc [TBinder, TAny, TBindings mempty])
  , ("def", TFunc [TList TBinder, TAny, TFunc [TList TBinder, TAny]])
  , ("merge", TFunc [TList (TBindings mempty), TBindings mempty])
  , ("++", TFunc [TString, TString, TString])
  , ("if", TFunc [TBool, TAny, TAny])
  , ("==", TFunc [TAny, TAny, TBool])
  ]
    where embed (x, t) = (x, Builtin t x)
