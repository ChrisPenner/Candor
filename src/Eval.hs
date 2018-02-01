{-# language ViewPatterns #-}
module Eval where

import RIO
import AST
import qualified Data.Map as M

type Bindings = Map String AST

primitives :: Bindings
primitives = M.fromList . fmap embed $ 
  [ "+" , "-" , "*" , "=" , "merge" ]
    where embed x = (x, Builtin x)

eval :: AST -> Either String AST
eval ast = eval' primitives ast

eval' :: Bindings -> AST -> Either String AST
eval' bindings (Symbol name) =
  case M.lookup name bindings of
    Just expr -> eval' bindings expr
    Nothing -> error $ "no symbol in scope for: " ++ name

eval' bindings (Appl h []) = eval' bindings h
eval' bindings (Appl ((eval' bindings) -> Right (Bindings binds)) [expr]) = eval' (bindings <> binds) expr
eval' bindings (Appl ((eval' bindings) -> Right (Builtin name)) args) =
  (traverse (eval' bindings) args) >>= builtin name 
eval' bindings (Appl ((eval' bindings) -> Right (Func binders expr)) args) = do
  newArgs <- traverse (eval' bindings) args
  argSymbols <- traverse assertBinders binders
  let newBindings =  bindings <> M.fromList (zip argSymbols newArgs)
  eval' newBindings expr
eval' _ (Appl expr _) =
  Left $ "expected function not expression: " ++ show expr
eval' bindings (List elems) = List <$> (traverse (eval' bindings) elems)
eval' _ f@(Func _ _) = Right $ f
eval' _ s@(Str _) = Right $ s
eval' _ n@(Number _) = Right $ n
eval' _ b@(Binder _) = Right $ b
eval' _ b@(Builtin _) = Right $ b
eval' _ b@(Bindings _) = Right $ b

builtin :: String -> [AST] -> Either String AST
builtin "+" [Number a, Number b] = Right $ Number (a + b)
builtin "+" args = Left $ "Expected two Number arguments to + but got:" ++ show args
builtin "-" [Number a, Number b] = Right $ Number (a - b)
builtin "-" args = Left $ "Expected two Number arguments to - but got:" ++ show args
builtin "*" [Number a, Number b] = Right $ Number (a * b)
builtin "*" args = Left $ "Expected two Number arguments to * but got:" ++ show args
builtin "=" [Binder name, expr] = Right $ Bindings (M.singleton name expr)
builtin "=" args = Left $ "Expected single expression argument to = but got:" ++ show args
builtin "merge" [List binds] = do
  allBindings <- traverse assertBindings binds
  return . Bindings $ M.unions allBindings
  where
    assertBindings (Bindings b) = Right b
    assertBindings b = Left $ "expected bindings; found: " ++ show b
builtin "merge" args = Left $ "Expected a single list argument to merge but got: " ++ show args
builtin name args = Left $ "no symbol in scope for " ++ name ++ ": " ++ show args

assertBinders :: AST -> Either String String
assertBinders (Binder name) = Right name
assertBinders b = Left $ "expected binding symbol; found: " ++ show b
