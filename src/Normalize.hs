{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

-- | Prepare the AST for conversion to bytecode
--
-- Resolves symbols to expressions, epxands 'Appl' nodes according to type, etc.
module Normalize where

import qualified AST
import Control.Lens (ifor, makeLenses, (+~))
import Control.Monad.Except
import Control.Monad.Writer.Strict
import qualified Data.Map as M
import qualified Data.Map as Map
import Primitives
import RIO

type Bindings = Map String Norm

newtype FuncName = FuncName Int
  deriving stock (Show, Eq, Ord)

newtype FuncDef = FuncDef (FuncName, Norm, [Int])

data Env = Env
  { _scopedBindings :: Bindings,
    _nextFuncArg :: Int
  }

type NormalizeM a = WriterT [FuncDef] (ReaderT Env (Either String)) a

data Norm
  = Str String
  | Number Int
  | Boolean Bool
  | List [Norm]
  | Builtin String
  | Bindings (Map String Norm)
  | If Norm Norm Norm
  | Add Norm Norm
  | Sub Norm Norm
  | Mul Norm Norm
  | StringAppend Norm Norm
  | Eq Norm Norm
  | FuncApp FuncName [Norm]
  | FuncLookup FuncName
  | ArgLabel Int
  deriving stock (Show, Eq)

makeLenses ''Env

normPrimitives :: Bindings
normPrimitives = Builtin <$> M.fromSet id (M.keysSet primitiveTypes)

eval :: AST.AST -> Either String (Norm, [FuncDef])
eval ast = runReaderT (runWriterT $ eval' ast) $ Env normPrimitives 0

eval' :: AST.AST -> NormalizeM Norm
eval' (AST.Symbol name) = do
  res <- asks (M.lookup name . _scopedBindings)
  case res of
    Just expr -> pure expr
    Nothing -> throwError $ "no symbol in scope for: " ++ name
eval' (AST.Appl h []) = eval' h
eval' (AST.Appl f args) = do
  appl <- eval' f
  case appl of
    Bindings newBinds -> do
      applyBindings newBinds args
    Builtin name -> builtin name args
    FuncLookup name -> do
      args' <- traverse eval' args
      pure $ FuncApp name args'
    _ -> throwError $ "expected function not expression: " ++ show f
eval' (AST.List elems) = List <$> traverse eval' elems
eval' (AST.Number n) = pure $ Number n
eval' (AST.Str s) = pure $ Str s
eval' (AST.Boolean b) = pure $ Boolean b
eval' (AST.FuncDef binders body) = funcDef binders body
eval' x = throwError $ "unhandled in Norm.eval: " ++ show x

applyBindings :: Bindings -> [AST.AST] -> NormalizeM Norm
applyBindings newBinds [expr] = local (scopedBindings %~ (newBinds <>)) $ eval' expr
applyBindings _ _ = throwError "expected single arg to Binding expression"

funcDef :: NonEmpty String -> AST.AST -> NormalizeM Norm
funcDef binders body = do
  currentFuncArg <- view nextFuncArg
  argBindings <- ifor binders $ \n binder -> do
    pure (binder, ArgLabel (currentFuncArg + n))
  local
    ( \env ->
        env
          & (scopedBindings %~ \binds -> Map.fromList (toList argBindings) <> binds)
          & nextFuncArg +~ length binders
    )
    $ do eval' body

builtin :: String -> [AST.AST] -> NormalizeM Norm
builtin "+" [a, b] = Add <$> eval' a <*> eval' b
builtin "-" [a, b] = Sub <$> eval' a <*> eval' b
builtin "*" [a, b] = Mul <$> eval' a <*> eval' b
builtin "++" [a, b] = StringAppend <$> eval' a <*> eval' b
builtin "==" [a, b] = Eq <$> eval' a <*> eval' b
builtin "merge" args = merge args
builtin "if" [cond, a, b] = If <$> eval' cond <*> eval' a <*> eval' b
builtin name _args = notFound name

if' :: [AST.AST] -> NormalizeM Norm
if' [p, x, y] = do
  res <- eval' p
  case res of
    Boolean True -> eval' x
    Boolean False -> eval' y
    _ -> throwError $ "Expected a Boolean predicate, got: " ++ show res
if' args = throwError $ "Expected a Boolean, then two expressions; got:" ++ show args

merge :: [AST.AST] -> NormalizeM Norm
merge [lst] = do
  lst' <- eval' lst
  case lst' of
    List binds -> do
      allBindings <- traverse assertBindings binds
      return . Bindings $ M.unions allBindings
    args -> throwError $ "Expected single list argument to merge but got:" ++ show args
merge args = throwError $ "Expected single list argument to merge but got:" ++ show args

assertBindings :: Norm -> NormalizeM (Map String Norm)
assertBindings (Bindings b) = return b
assertBindings b = throwError $ "expected bindings; found: " ++ show b

notFound :: String -> NormalizeM a
notFound name = throwError $ "no symbol in scope for " ++ name

assertSymbols :: AST.AST -> NormalizeM String
assertSymbols (AST.Symbol name) = return name
assertSymbols b = throwError $ "expected binding symbol; found: " ++ show b
