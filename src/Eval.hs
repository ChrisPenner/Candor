{-# language ViewPatterns #-}
module Eval where

import RIO
import AST
import qualified Data.Map as M
import Primitives
import Control.Monad.Except
import Debug.Trace (trace)

eval :: AST -> Either String AST
eval ast = runReaderT (eval' ast) primitives

eval' :: AST -> EvalM AST
eval' (Symbol name) = do
  res <- asks (M.lookup name)
  case res of
    Just expr -> trace ("found name: " ++ name ++ ": " ++ show expr) $ eval' expr
    Nothing -> throwError $ "no symbol in scope for: " ++ name
eval' (Appl h []) = eval' h
eval' (Appl f args) = do
  appl <- trace ("evaluating APPL head: " ++ show f) (eval' f)
  case appl of
    Bindings newBinds -> bindings newBinds args
    Builtin _ name -> trace ("evalling builtin: " ++ show name) $ builtin name args
    FuncDef binders expr -> func binders args expr
    _ ->  throwError $ "expected function not expression: " ++ show f
eval' (List elems) = List <$> traverse eval' elems
eval' v = return v

bindings :: Bindings -> [AST] -> EvalM AST
bindings newBinds [expr] = trace ("evalling: " ++ show expr ++ " with: " ++ show newBinds) local (newBinds <>) $ eval' expr
bindings _ _ = throwError "expected single arg to Binding expression"

func :: [String] -> [AST] -> AST -> EvalM AST
func binders args expr = do
  evalArgs <- traverse eval' args
  let newBindings = M.fromList $ zip binders evalArgs
  res <- trace ("Running Function with args: " ++ show evalArgs ++ "\n and EXPR: " ++ show expr) (local (newBindings <>) $ eval' expr)
  trace ("got result: " ++ show res) (return res)

builtin :: String -> [AST] -> EvalM AST
builtin "+" = numBinOp (+)
builtin "-" = numBinOp subtract
builtin "*" = numBinOp (*)
builtin "++" = stringBinOp (++)
builtin "=" = eq'
builtin "=="  = eqBool
builtin "merge" = merge
builtin "def" = def
builtin "if" = if'
builtin name = notFound name

def :: [AST] -> EvalM AST
def args@[binders, expr] = do
  bindStrings <- case binders of
    List binders' -> traverse assertBinders binders'
    _ -> throwError $ "expected list of binders, then expression; got: " ++ show args
  trace ("defined new function: " ++ show (FuncDef bindStrings expr)) . return $ FuncDef bindStrings expr
def args = throwError $ "expected list of binders, then an expression; got: " ++ show args

if' :: [AST] -> EvalM AST
if' [p, x, y] = do
  res <- eval' p
  case res of
    Boolean True -> trace ("if was true, evalling: " ++ show x) $ eval' x
    Boolean False -> trace ("if was false, evalling: " ++ show x) $ eval' y
    _ -> throwError $ "Expected a Boolean predicate, got: " ++ show res
if' args = throwError $ "Expected a Boolean, then two expressions; got:" ++ show args

eq' :: [AST] -> EvalM AST
eq' [Binder name, expr] = do
  res <- eval' expr
  trace ("assigning " ++ name ++ " to: " ++ show expr) (return $ Bindings (M.singleton name res))
eq' args = throwError $ "Expected binder and expression argument to = but got:" ++ show args

numBinOp :: (Int -> Int -> Int) -> [AST] -> EvalM AST
numBinOp f [x, y] = do
  bindMap <- ask
  res <- trace ("evalling args: " ++ show [x, y] ++ " with: " ++ show bindMap) $ traverse eval' [x, y]
  case trace ("resolved args to: " ++ show res) res of
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
eqBool  [a, b] = do
  a' <- eval' a
  b' <- eval' b
  return $ Boolean (a' == b')
eqBool _ = return $ Boolean False

merge :: [AST] -> EvalM AST
merge  [lst] = do
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

assertBinders :: AST -> EvalM String
assertBinders (Binder name) = return name
assertBinders b = throwError $ "expected binding symbol; found: " ++ show b
