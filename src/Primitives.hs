module Primitives where

import AST
import Types

import RIO
import qualified Data.Map as M

primitives :: Bindings
primitives = M.fromList . fmap embed $
  [ ("+", TFunc [TNumber, TNumber, TNumber])
  , ("-", TFunc [TNumber, TNumber, TNumber])
  , ("*", TFunc [TNumber, TNumber, TNumber])
  , ("=", TFunc [TBinder, TAny, TBindings mempty])
  , ("merge", TFunc [TList [TBindings mempty], TBindings mempty])
  , ("++", TFunc [TString, TString, TString])
  , ("if", TFunc [TBool, TAny, TAny])
  , ("==", TFunc [TAny, TAny, TBool])
  ]
    where embed (x, t) = (x, Builtin t x)

numBinOp :: (Int -> Int -> Int) -> [AST] -> Either String AST
numBinOp f [Number x, Number y] = Right (Number $ f x y)
numBinOp _ args = Left $ "expected 2 numbers, got: " ++ show args

stringBinOp :: (String -> String -> String) -> [AST] -> Either String AST
stringBinOp f [Str x, Str y] = Right (Str $ f x y)
stringBinOp _ args = Left $ "expected 2 strings, got: " ++ show (length args)

if' :: [AST] -> Either String AST
if' [Boolean True, x, _] = Right x
if' [Boolean False, _, y] = Right y
if' args = Left $ "Expected a Boolean, then two expressions; got:" ++ show args

eq' :: [AST] -> Either String AST
eq' [Binder name, expr] = Right $ Bindings (M.singleton name expr)
eq' args = Left $ "Expected binder and expression argument to = but got:" ++ show args

eqBool :: [AST] -> Either String AST
eqBool  [a, b] = Right $ Boolean (a == b)
eqBool _ = Right $ Boolean False

merge :: [AST] -> Either String AST
merge  [List binds] = do
      allBindings <- traverse assertBindings binds
      return . Bindings $ M.unions allBindings
merge args = Left $ "Expected single list argument to merge but got:" ++ show args

assertBindings :: AST -> Either String (Map String AST)
assertBindings (Bindings b) = Right b
assertBindings b = Left $ "expected bindings; found: " ++ show b

notFound :: String -> [AST] -> Either String AST
notFound name args = Left $ "no symbol in scope for " ++ name ++ ": " ++ show args 

builtin :: String -> [AST] -> Either String AST
builtin "if" = if'
builtin "+" = numBinOp (+)
builtin "-" = numBinOp (-)
builtin "*" = numBinOp (*)
builtin "++" = stringBinOp (++)
builtin "=" = eq'
builtin "=="  = eqBool
builtin "merge" = merge
builtin name = notFound name

assertBinders :: AST -> Either String String
assertBinders (Binder name) = Right name
assertBinders b = Left $ "expected binding symbol; found: " ++ show b
