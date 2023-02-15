{-# LANGUAGE OverloadedLists #-}

module Primitives where

import AST
import qualified Data.Map as M
import RIO
import Types

primitives :: Bindings
primitives = Builtin <$> M.fromSet id (M.keysSet primitiveTypes)

primitiveTypes :: M.Map Text Monotype
primitiveTypes =
  [ ("+", TFunc intT (TFunc intT intT)),
    ("-", TFunc intT (TFunc intT intT)),
    ("*", TFunc intT (TFunc intT intT)),
    ("++", TFunc stringT (TFunc stringT stringT)),
    ("if", TFunc boolT (TFunc varT (TFunc varT varT))),
    ("==", TFunc varT (TFunc varT boolT))
  ]
