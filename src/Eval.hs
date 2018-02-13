{-# language ViewPatterns #-}
module Eval where

import RIO
import AST
import qualified Data.Map as M
import Primitives

eval :: AST -> Either String AST
eval = eval' primitives

eval' :: Bindings -> AST -> Either String AST
eval' bindings (Symbol name) =
  case M.lookup name bindings of
    Just expr -> eval' bindings expr
    Nothing -> Left $ "no symbol in scope for: " ++ name
eval' bindings (Appl h []) = eval' bindings h
eval' bindings (Appl f args) = do
  appl <- eval' bindings f
  case appl of
    Bindings newBinds ->
      case args of
        [expr] -> eval' (bindings <> newBinds) expr
        _ -> Left "expected single arg to Binding expression"
    Builtin _ name -> builtin bindings name args >>= eval' bindings
    FuncDef binders expr -> do
      evalArgs <- traverse (eval' bindings) args
      let newBindings = M.fromList $ zip binders evalArgs
      eval' (bindings <> newBindings) expr
    _ ->  Left $ "expected function not expression: " ++ show f
eval' bindings (List elems) = List <$> traverse (eval' bindings) elems
eval' _ v = Right v

builtin :: Bindings -> String -> [AST] -> Either String AST
builtin bindings "def" args = def bindings args
builtin bindings "if" args = if' bindings args
builtin bindings name args = do
  evalArgs <- traverse (eval' bindings) args
  builtin' name evalArgs

builtin' :: String -> [AST] -> Either String AST
builtin' "+" = numBinOp (+)
builtin' "-" = numBinOp (-)
builtin' "*" = numBinOp (*)
builtin' "++" = stringBinOp (++)
builtin' "=" = eq'
builtin' "=="  = eqBool
builtin' "merge" = merge
builtin' name = notFound name

def :: Bindings -> [AST] -> Either String AST
def bindings args@[b, expr] = do
  binders <- eval' bindings b
  bindStrings <- case binders of
    List binders' -> traverse assertBinders binders'
    _ -> Left $ "expected list of binders, then expression; got: " ++ show args
  return $ FuncDef bindStrings expr
def _ args = Left $ "expected list of binders, then an expression; got: " ++ show args

if' :: Bindings -> [AST] -> Either String AST
if' bindings args@[p, x, y] = do
  res <- eval' bindings p
  case res of
    Boolean True -> eval' bindings x
    Boolean False -> eval' bindings y
    _ -> Left $ "Expected a Boolean, then two expressions; got:" ++ show args
if' _ args = Left $ "Expected a Boolean, then two expressions; got:" ++ show args


eq' :: [AST] -> Either String AST
eq' [Binder name, expr] = Right $ Bindings (M.singleton name expr)
eq' args = Left $ "Expected binder and expression argument to = but got:" ++ show args

