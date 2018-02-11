{-# language ViewPatterns #-}
module Eval where

import RIO
import AST
import qualified Data.Map as M
import Primitives

eval :: AST -> Either String AST
eval ast = eval' primitives ast

eval' :: Bindings -> AST -> Either String AST
eval' bindings (Symbol name) =
  case M.lookup name bindings of
    Just expr -> eval' bindings expr
    Nothing -> Left $ "no symbol in scope for: " ++ name

eval' bindings (Appl h []) = eval' bindings h
eval' bindings (Appl ((eval' bindings) -> Right (Bindings binds)) [expr]) = eval' (bindings <> binds) expr
eval' bindings (Appl ((eval' bindings) -> Right (Builtin _ name)) args) = builtin name bindings args
eval' bindings (Appl ((eval' bindings) -> Right (FuncDef binders expr)) args) = do
  newArgs <- traverse (eval' bindings) args
  argSymbols <- traverse assertBinders binders
  let newBindings =  bindings <> M.fromList (zip argSymbols newArgs)
  eval' newBindings expr
eval' _ (Appl expr _) =
  Left $ "expected function not expression: " ++ show expr
eval' bindings (List elems) = List <$> (traverse (eval' bindings) elems)
eval' _ f@(FuncDef _ _) = Right $ f
eval' _ s@(Str _) = Right $ s
eval' _ n@(Number _) = Right $ n
eval' _ b@(Binder _) = Right $ b
eval' _ b@(Builtin _ _) = Right $ b
eval' _ b@(Bindings _) = Right $ b
eval' _ b@(Boolean _) = Right $ b

binOp :: (Int -> Int -> Int) -> Bindings -> [AST] -> Either String AST
binOp f bindings [x, y] = do
  a <- eval' bindings x
  b <- eval' bindings y
  case (a, b) of
    (Number a', Number b') -> return . Number $ f a' b'
    (a', b') -> Left $ "expected numbers, got: " ++ show (a', b')
binOp f bindings args = Left $ "expected 2 args, got: " ++ show (length args)

stringBinOp :: (String -> String -> String) -> Bindings -> [AST] -> Either String AST
stringBinOp f bindings [x, y] = do
  a <- eval' bindings x
  b <- eval' bindings y
  case (a, b) of
    (Str a', Str b') -> return . Str $ f a' b'
    (a', b') -> Left $ "expected strings, got: " ++ show (a', b')
stringBinOp _ _ args = Left $ "expected 2 args, got: " ++ show (length args)

if' :: Bindings -> [AST] -> Either String AST
if' bindings [b, x, y] = do
  res <- eval' bindings b
  case res of
    Boolean True -> eval' bindings x
    Boolean False -> eval' bindings y
    wut -> Left $ "Expected a boolean expression to 'if' but got:" ++ show wut
if' _ args = Left $ "Expected a condition then two expressions; got:" ++ show args

eq' :: Bindings -> [AST] -> Either String AST
eq' bindings [binder, expr] = do
  res <- eval' bindings binder
  case res of
    Binder name -> return $ Bindings (M.singleton name expr)
    wut -> Left $ "expected binder for (=), got: " ++ show wut
eq' _ args = Left $ "Expected single expression argument to = but got:" ++ show args

eqBool :: Bindings -> [AST] -> Either String AST
eqBool bindings [a, b] = Boolean <$> ((==) <$> eval' bindings a <*> eval' bindings b)
eqBool _ _ = Right $ Boolean False

merge ::  Bindings -> [AST] -> Either String AST
merge bindings [a] = do
  res <- eval' bindings a
  case res of
    List binds -> do
      allBindings <- traverse assertBindings binds
      return . Bindings $ M.unions allBindings
    wut -> Left $ "expected bindings for (merge), got: " ++ show wut
merge _ args = Left $ "Expected single list argument to merge but got:" ++ show args

assertBindings :: AST -> Either String (Map String AST)
assertBindings (Bindings b) = Right b
assertBindings b = Left $ "expected bindings; found: " ++ show b

notFound :: String -> Bindings -> [AST] -> Either String AST
notFound name _ args = Left $ "no symbol in scope for " ++ name ++ ": " ++ show args 

builtin :: String -> Bindings -> [AST] -> Either String AST
builtin "if" = if'
builtin "+" = binOp (+)
builtin "-" = binOp (-)
builtin "*" = binOp (*)
builtin "++" = stringBinOp (++)
builtin "=" = eq'
builtin "=="  = eqBool
builtin "merge" = merge
builtin name = notFound name

assertBinders :: AST -> Either String String
assertBinders (Binder name) = Right name
assertBinders b = Left $ "expected binding symbol; found: " ++ show b
