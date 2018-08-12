{-# LANGUAGE ViewPatterns #-}

module Eval where

import AST
import Control.Monad.Except
import Data.Functor.Foldable
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.Map as M
import Primitives
import RIO

eval :: AST -> Either String AST
eval ast = runReaderT (eval' ast) primitives

eval' :: AST -> EvalM AST
eval' (unfix -> Symbol name) = do
  res <- asks (M.lookup name)
  case res of
    Just expr -> eval' expr
    Nothing -> throwError $ "no symbol in scope for: " ++ name
eval' (unfix -> Appl h []) = eval' h
eval' (unfix -> Appl f args) = do
  appl <- eval' f
  case unfix appl of
    Bindings newBinds -> bindings newBinds args
    Builtin name -> builtin name args
    FuncDef binders expr -> func binders args expr
    _ -> throwError $ "expected function not expression: " ++ pretty f
eval' (unfix -> List elems) = Fix . List <$> traverse eval' elems
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
  case unfix res of
    Boolean True -> eval' x
    Boolean False -> eval' y
    _ -> throwError $ "Expected a Boolean predicate, got: " ++ pretty res
if' args =
  throwError $ "Expected a Boolean, then two expressions; got:" ++ pretty args

numBinOp :: (Int -> Int -> Int) -> [AST] -> EvalM AST
numBinOp f [x, y] = do
  res <- traverse eval' [x, y]
  case unfix <$> res of
    [Number x', Number y'] -> return . Fix $ Number (f x' y')
    args -> throwError $ "expected 2 number args, got:" ++ pretty args
numBinOp _ args = throwError $ "expected 2 numbers, got: " ++ pretty args

stringBinOp :: (String -> String -> String) -> [AST] -> EvalM AST
stringBinOp f [x, y] = do
  res <- traverse eval' [x, y]
  case unfix <$> res of
    [Str x', Str y'] -> return . Fix $ Str (f x' y')
    args -> throwError $ "expected 2 string args, got:" ++ pretty args
stringBinOp _ args =
  throwError $ "expected 2 strings, got: " ++ show (length args)

eqBool :: [AST] -> EvalM AST
eqBool [a, b] = do
  a' <- eval' a
  b' <- eval' b
  return . Fix $ Boolean (a' == b')
eqBool _ = return . Fix $ Boolean False

merge :: [AST] -> EvalM AST
merge [lst] = do
  lst' <- eval' lst
  case unfix lst' of
    List binds -> do
      allBindings <- traverse assertBindings binds
      return . Fix . Bindings $ M.unions allBindings
    args ->
      throwError $
      "Expected single list argument to merge but got:" ++ pretty args
merge args =
  throwError $ "Expected single list argument to merge but got:" ++ pretty args

assertBindings :: AST -> EvalM (Map String AST)
assertBindings (unfix -> Bindings b) = return b
assertBindings b = throwError $ "expected bindings; found: " ++ pretty b

notFound :: String -> [AST] -> EvalM AST
notFound name args =
  throwError $ "no symbol in scope for " ++ name ++ ": " ++ pretty args

assertSymbols :: AST -> EvalM String
assertSymbols (unfix -> Symbol name) = return name
assertSymbols b = throwError $ "expected binding symbol; found: " ++ pretty b
