{-# language ViewPatterns #-}
module Eval where

import RIO
import AST
import qualified Data.Map as M
import Primitives
import Control.Monad.Reader
import Control.Monad.Except

eval :: AST -> Either String AST
eval ast = runReaderT (eval' ast) primitives 

eval' :: AST -> EvalM AST
eval' (Symbol name) = do
  res <- asks $ M.lookup name
  case res of
    Just expr -> return expr
    Nothing -> throwError $ "no symbol in scope for: " ++ name
eval' (Appl h []) = eval' h
eval' (Appl f args) = do
  appl <- eval' f
  case appl of
    Bindings newBinds ->
      case args of
        [expr] -> local (<> newBinds) $ eval' expr
        _ -> throwError "expected single arg to Binding expression"
    Builtin _ name -> builtin name args
    FuncDef binders expr -> do
      evalArgs <- traverse eval' args
      let newBindings = M.fromList $ zip binders evalArgs
      local (<> newBindings) $ eval' expr
    _ ->  throwError $ "expected function not expression: " ++ show f
eval' (List elems) = List <$> traverse eval' elems
eval' v = return v

builtin :: String -> [AST] -> EvalM AST
builtin "def" args = def args
builtin "if" args = if' args
builtin name args = do
  evalArgs <- traverse eval' args
  builtin' name evalArgs

builtin' :: String -> [AST] -> EvalM AST
builtin' "+" = numBinOp (+)
builtin' "-" = numBinOp (-)
builtin' "*" = numBinOp (*)
builtin' "++" = stringBinOp (++)
builtin' "=" = eq'
builtin' "=="  = eqBool
builtin' "merge" = merge
builtin' name = notFound name

def :: [AST] -> EvalM AST
def args@[b, expr] = do
  binders <- eval' b
  bindStrings <- case binders of
    List binders' -> traverse assertBinders binders'
    _ -> throwError $ "expected list of binders, then expression; got: " ++ show args
  return $ FuncDef bindStrings expr
def args = throwError $ "expected list of binders, then an expression; got: " ++ show args

if' :: [AST] -> EvalM AST
if' args@[p, x, y] = do
  res <- eval' p
  case res of
    Boolean True -> eval' x
    Boolean False -> eval' y
    _ -> throwError $ "Expected a Boolean, then two expressions; got:" ++ show args
if' args = throwError $ "Expected a Boolean, then two expressions; got:" ++ show args


eq' :: [AST] -> EvalM AST
eq' [Binder name, expr] = return $ Bindings (M.singleton name expr)
eq' args = throwError $ "Expected binder and expression argument to = but got:" ++ show args

