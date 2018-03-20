{-# language OverloadedLists #-}
module Primitives where

import AST

import RIO
import Data.Bifunctor as Bi
import TypeInference
import qualified Data.Map as M

primitives :: Bindings
primitives = Builtin <$> M.fromSet id (M.keysSet primitiveTypes)

primitiveTypes :: M.Map String Polytype
primitiveTypes =
  [ ("+", Forall mempty $ TFunc intT (TFunc intT intT))
  , ("-", Forall mempty $ TFunc intT (TFunc intT intT))
  , ("*", Forall mempty $ TFunc intT (TFunc intT intT))
  , ("=", Forall mempty $ TFunc binderT (TFunc varT bindingsT))
  , ("def", Forall mempty $ TFunc (TList binderT) varT)
  , ("merge", Forall mempty $ TFunc (TList bindingsT) bindingsT)
  , ("++", Forall mempty $ TFunc stringT (TFunc stringT stringT))
  , ("if", Forall mempty $ TFunc boolT (TFunc varT (TFunc varT varT)))
  , ("==", Forall mempty $ TFunc varT (TFunc varT boolT))
  ]
