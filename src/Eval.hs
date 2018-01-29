{-# language ViewPatterns #-}
module Eval where

import RIO
import AST
import qualified Data.Map as M

type Bindings = Map String AST

globals :: Bindings
globals = M.fromList
            [ ("+", Builtin "+")
            , ("-", Builtin "-")
            , ("*", Builtin "*")
            , ("=", Builtin "=")
            , ("merge", Builtin "merge")
            ]

eval :: AST -> AST
eval ast = eval' globals ast

eval' :: Bindings -> AST -> AST
eval' bindings (Symbol name) =
  case M.lookup name bindings of
    Just expr -> eval' bindings expr
    Nothing -> error $ "no symbol in scope for: " ++ name

eval' bindings (Appl h []) = eval' bindings h
eval' bindings (Appl ((eval' bindings) -> Bindings binds) [expr]) = eval' (bindings <> binds) expr
eval' bindings (Appl ((eval' bindings) -> Builtin name) args) =
  builtin name (eval' bindings <$> args)
eval' bindings (Appl ((eval' bindings) -> Func binders expr) args) =
  let argSymbols = assertBinders <$> binders
      newBindings = bindings <> M.fromList (zip argSymbols (eval' bindings <$> args))
    in eval' newBindings expr
eval' _ (Appl expr _) =
  error $ "expected function not expression: " ++ show expr
eval' bindings (List elems) = List (eval' bindings <$> elems)
eval' _ f@(Func _ _) = f
eval' _ s@(Str _) = s
eval' _ n@(Number _) = n
eval' _ b@(Binder _) = b
eval' _ b@(Builtin _) = b
eval' _ b@(Bindings _) = b

builtin :: String -> [AST] -> AST
builtin "+" [Number a, Number b] = Number (a + b)
builtin "-" [Number a, Number b] = Number (a - b)
builtin "*" [Number a, Number b] = Number (a * b)
builtin "=" [Binder name, expr] = Bindings (M.singleton name expr)
builtin "merge" [List binds] =
  let allBindings = assertBindings <$> binds
   in Bindings (M.unions allBindings)
  where
    assertBindings (Bindings b) = b
    assertBindings b = error $ "expected bindings; found: " ++ show b
builtin name args = error $ "no symbol in scope for " ++ name ++ ": " ++ show args

assertBinders :: AST -> String
assertBinders (Binder name) = name
assertBinders b = error $ "expected binding symbol; found: " ++ show b
