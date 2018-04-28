{-# LANGUAGE OverloadedLists #-}

module Primitives where

import AST

import RIO
import Types
import qualified Data.Map as M

primitives :: Bindings
primitives = Builtin <$> M.fromSet id (M.keysSet primitiveTypes)

primitiveTypes :: M.Map String Monotype
primitiveTypes =
  [ ("+", TFunc intT (TFunc intT intT))
  , ("-", TFunc intT (TFunc intT intT))
  , ("*", TFunc intT (TFunc intT intT))
  , ("++", TFunc stringT (TFunc stringT stringT))
  , ("if", TFunc boolT (TFunc varT (TFunc varT varT)))
  , ("==", TFunc varT (TFunc varT boolT))
  ]
