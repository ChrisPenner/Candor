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

eval :: AST -> Either String AST
eval = eval' . subBindings primitives

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
    Builtin _ name -> traverse eval' args >>= builtin name
    FuncDef binders expr -> do
      argSymbols <- traverse assertBinders binders
      evalArgs <- traverse eval' args
      let newBindings = M.fromList $ zip argSymbols evalArgs
      eval' (subBindings newBindings expr)
    _ ->  Left $ "expected function not expression: " ++ show f
eval' (List elems) = List <$> (traverse eval' elems)
eval' v = Right v
