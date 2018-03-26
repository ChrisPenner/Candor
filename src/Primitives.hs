{-# language OverloadedLists #-}
module Primitives where

import AST

import RIO
import Types
import qualified Data.Map as M

primitives :: Bindings
primitives = Builtin <$> M.fromSet id (M.keysSet primitiveTypes)

primitiveTypes :: M.Map String Polytype
primitiveTypes =
  [ ("+", Forall mempty $ TFunc intT (TFunc intT intT))
  , ("-", Forall mempty $ TFunc intT (TFunc intT intT))
  , ("*", Forall mempty $ TFunc intT (TFunc intT intT))
  , ("merge", Forall mempty $ TFunc (TList (TBindings [])) (TBindings []))
  , ("++", Forall mempty $ TFunc stringT (TFunc stringT stringT))
  , ("if", Forall mempty $ TFunc boolT (TFunc varT (TFunc varT varT)))
  , ("==", Forall mempty $ TFunc varT (TFunc varT boolT))
  ]
