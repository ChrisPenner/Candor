module Primitives where

import AST

import RIO
import qualified Data.Map as M

primitives :: Bindings
primitives = M.fromList . fmap embed $
  [ ("+", TFunc intT (TFunc intT intT))
  , ("-", TFunc intT (TFunc intT intT))
  , ("*", TFunc intT (TFunc intT intT))
  , ("=", TFunc binderT (TFunc varT bindingsT))
  , ("def", TFunc (TList binderT) varT)
  , ("merge", TFunc (TList bindingsT) bindingsT)
  , ("++", TFunc stringT (TFunc stringT stringT))
  , ("if", TFunc boolT (TFunc varT (TFunc varT varT)))
  , ("==", TFunc varT (TFunc varT boolT))
  ]
    where embed (x, t) = (x, Builtin t x)
