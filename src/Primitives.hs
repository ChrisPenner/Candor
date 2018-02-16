module Primitives where

import AST

import RIO
import Control.Monad.Except
import qualified Data.Map as M

primitives :: Bindings
primitives = M.fromList . fmap embed $
  [ ("+", TFunc [TNumber, TNumber, TNumber])
  , ("-", TFunc [TNumber, TNumber, TNumber])
  , ("*", TFunc [TNumber, TNumber, TNumber])
  , ("=", TFunc [TBinder, TAny, TBindings mempty])
  , ("def", TFunc [TList TBinder, TAny, TFunc [TList TBinder, TAny]])
  , ("merge", TFunc [TList (TBindings mempty), TBindings mempty])
  , ("++", TFunc [TString, TString, TString])
  , ("if", TFunc [TBool, TAny, TAny])
  , ("==", TFunc [TAny, TAny, TBool])
  ]
    where embed (x, t) = (x, Builtin t x)

numBinOp :: (Int -> Int -> Int) -> [AST] -> EvalM AST
numBinOp f [Number x, Number y] = return (Number $ f x y)
numBinOp _ args = throwError $ "expected 2 numbers, got: " ++ show args

stringBinOp :: (String -> String -> String) -> [AST] -> EvalM AST
stringBinOp f [Str x, Str y] = return (Str $ f x y)
stringBinOp _ args = throwError $ "expected 2 strings, got: " ++ show (length args)

eqBool :: [AST] -> EvalM AST
eqBool  [a, b] = return $ Boolean (a == b)
eqBool _ = return $ Boolean False

merge :: [AST] -> EvalM AST
merge  [List binds] = do
      allBindings <- traverse assertBindings binds
      return . Bindings $ M.unions allBindings
merge args = throwError $ "Expected single list argument to merge but got:" ++ show args

assertBindings :: AST -> EvalM (Map String AST)
assertBindings (Bindings b) = return b
assertBindings b = throwError $ "expected bindings; found: " ++ show b

notFound :: String -> [AST] -> EvalM AST
notFound name args = throwError $ "no symbol in scope for " ++ name ++ ": " ++ show args 

assertBinders :: AST -> EvalM String
assertBinders (Binder name) = return name
assertBinders b = throwError $ "expected binding symbol; found: " ++ show b
