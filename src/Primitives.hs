module Primitives where

import AST
import Types

import RIO
import qualified Data.Map as M

primitives :: Bindings
primitives = M.fromList . fmap embed $
  [ ("+", TFunc [TNumber, TNumber, TNumber])
  , ("-", TFunc [TNumber, TNumber, TNumber])
  , ("*", TFunc [TNumber, TNumber, TNumber])
  , ("=", TFunc [TBinder, TAny, TBindings mempty])
  , ("merge", TFunc [TList [TBindings mempty], TBindings mempty])
  , ("++", TFunc [TString, TString, TString])
  , ("if", TFunc [TBool, TAny, TAny])
  , ("==", TFunc [TAny, TAny, TBool])
  ]
    where embed (x, t) = (x, Builtin t x)
