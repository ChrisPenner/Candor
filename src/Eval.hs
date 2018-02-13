{-# language ViewPatterns #-}
module Eval where

import RIO
import AST
import qualified Data.Map as M
import Primitives

sub :: String -> AST -> AST -> AST
sub name v (Symbol n) | name == n = v
sub name v (Bindings bindings) = Bindings (sub name v <$> bindings)
sub name v (FuncDef names expr) = FuncDef names (sub name v expr)
sub name v (Appl f args) = Appl (sub name v f) (sub name v <$> args)
sub name v (List elems) = List (sub name v <$> elems)
sub _ _ x = x

subBindings :: Bindings -> AST -> AST
subBindings bindings = appEndo . foldMap Endo $ uncurry sub <$> M.toList bindings

handleBuiltins :: AST -> AST
handleBuiltins = subBindings primitives

eval :: AST -> Either String AST
eval = eval' . handleBuiltins

eval' :: AST -> Either String AST
eval' (Symbol name) = Left $ "no symbol in scope for: " ++ name
eval' (Appl h []) = eval' h
eval' (Appl f args) = do
  appl <- eval' f
  case appl of
    Bindings newBinds ->
      case args of
        [expr] -> eval' (subBindings newBinds expr)
        _ -> Left "expected single arg to Binding expression"
    Builtin _ name -> builtin name args
    FuncDef binders expr -> do
      evalArgs <- traverse eval' args
      let newBindings = M.fromList $ zip binders evalArgs
      eval' (subBindings newBindings expr)
    _ ->  Left $ "expected function not expression: " ++ show f
eval' (List elems) = List <$> (traverse eval' elems)
eval' v = Right v

builtin :: String -> [AST] -> Either String AST
builtin "def" args = def args
builtin name args = do
  evalArgs <- traverse eval' args
  builtin' name evalArgs

builtin' :: String -> [AST] -> Either String AST
builtin' "if" = if'
builtin' "+" = numBinOp (+)
builtin' "-" = numBinOp (-)
builtin' "*" = numBinOp (*)
builtin' "++" = stringBinOp (++)
builtin' "=" = eq'
builtin' "=="  = eqBool
builtin' "merge" = merge
builtin' "def" = def
builtin' name = notFound name

def :: [AST] -> Either String AST
def [List binders, expr] = do
  binds <- traverse assertBinders binders
  return $ FuncDef binds expr
def args = Left $ "expected list of binders, then an expression; got: " ++ show args

eq' :: [AST] -> Either String AST
eq' [Binder name, expr] = Right $ Bindings (M.singleton name (sub name expr expr))
eq' args = Left $ "Expected binder and expression argument to = but got:" ++ show args

