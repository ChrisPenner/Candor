{-# LANGUAGE ViewPatterns #-}

module Wasm.Eval where

import AST
import Control.Monad.Except
import qualified Data.Map as M
import Primitives
import RIO
import qualified Wasm

ast2Wat :: AST -> Text
ast2Wat ast = runIdentity . Wasm.renderModule $ ast2Module ast

ast2Module :: AST -> Wasm.Wasm Wasm.Declaration m i o a
ast2Module = _

eval' :: AST -> EvalM AST
eval' (Symbol name) = do
  res <- asks (M.lookup name)
  case res of
    Just expr -> eval' expr
    Nothing -> throwError $ "no symbol in scope for: " ++ name
eval' (Appl h []) = eval' h
eval' (Appl f args) = do
  appl <- eval' f
  case appl of
    Bindings newBinds -> bindings newBinds args
    Builtin name -> builtin name args
    FuncDef binders expr -> func binders args expr
    _ -> throwError $ "expected function not expression: " ++ show f
eval' (List elems) = List <$> traverse eval' elems
eval' v = return v

bindings :: Bindings -> [AST] -> EvalM AST
bindings newBinds [expr] = local (newBinds <>) $ eval' expr
bindings _ _ = throwError "expected single arg to Binding expression"

func :: NonEmpty String -> [AST] -> AST -> EvalM AST
func (toList -> binders) args expr = do
  evalArgs <- traverse eval' args
  let newBindings = M.fromList $ zip binders evalArgs
  local (newBindings <>) $ eval' expr

builtin :: String -> [AST] -> EvalM AST
builtin "+" = numBinOp (+)
builtin "-" = numBinOp (-)
builtin "*" = numBinOp (*)
builtin "++" = stringBinOp (++)
builtin "==" = eqBool
builtin "merge" = merge
builtin "if" = if'
builtin name = notFound name

if' :: [AST] -> EvalM AST
if' [p, x, y] = do
  res <- eval' p
  case res of
    Boolean True -> eval' x
    Boolean False -> eval' y
    _ -> throwError $ "Expected a Boolean predicate, got: " ++ show res
if' args = throwError $ "Expected a Boolean, then two expressions; got:" ++ show args

numBinOp :: (Int -> Int -> Int) -> [AST] -> EvalM AST
numBinOp f [x, y] = do
  res <- traverse eval' [x, y]
  case res of
    [Number x', Number y'] -> return $ Number (f x' y')
    args -> throwError $ "expected 2 number args, got:" ++ show args
numBinOp _ args = throwError $ "expected 2 numbers, got: " ++ show args

stringBinOp :: (String -> String -> String) -> [AST] -> EvalM AST
stringBinOp f [x, y] = do
  res <- traverse eval' [x, y]
  case res of
    [Str x', Str y'] -> return $ Str (f x' y')
    args -> throwError $ "expected 2 string args, got:" ++ show args
stringBinOp _ args = throwError $ "expected 2 strings, got: " ++ show (length args)

eqBool :: [AST] -> EvalM AST
eqBool [a, b] = do
  a' <- eval' a
  b' <- eval' b
  return $ Boolean (a' == b')
eqBool _ = return $ Boolean False

merge :: [AST] -> EvalM AST
merge [lst] = do
  lst' <- eval' lst
  case lst' of
    List binds -> do
      allBindings <- traverse assertBindings binds
      return . Bindings $ M.unions allBindings
    args -> throwError $ "Expected single list argument to merge but got:" ++ show args
merge args = throwError $ "Expected single list argument to merge but got:" ++ show args

assertBindings :: AST -> EvalM (Map String AST)
assertBindings (Bindings b) = return b
assertBindings b = throwError $ "expected bindings; found: " ++ show b

notFound :: String -> [AST] -> EvalM AST
notFound name args = throwError $ "no symbol in scope for " ++ name ++ ": " ++ show args

assertSymbols :: AST -> EvalM String
assertSymbols (Symbol name) = return name
assertSymbols b = throwError $ "expected binding symbol; found: " ++ show b
