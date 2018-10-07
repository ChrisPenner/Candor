{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}

module Primitives where

import AST
import Data.Functor.Foldable

-- import Data.Functor.Foldable
import qualified Data.Map as M
import RIO

-- import Types
-- -- primitives :: Bindings
-- -- primitives = Fix . Builtin <$> M.fromSet id (M.keysSet primitiveTypes)
-- primitiveTypes :: M.Map String Monotype
-- primitiveTypes =
--   [ ("+", TFunc intT (TFunc intT intT))
--   , ("-", TFunc intT (TFunc intT intT))
--   , ("*", TFunc intT (TFunc intT intT))
--   , ("++", TFunc stringT (TFunc stringT stringT))
--   , ("if", TFunc boolT (TFunc varT (TFunc varT varT)))
--   , ("==", TFunc varT (TFunc varT boolT))
--   ]
runBuiltin :: String -> [NoBindingsAST] -> NoBindingsAST
runBuiltin "+" = intBinOp (+)
runBuiltin "-" = intBinOp (-)
runBuiltin "*" = intBinOp (*)
runBuiltin "if" = if'
runBuiltin "==" = eq'
runBuiltin x = error $ "unknown builtin: " ++ x

if' :: [NoBindingsAST] -> NoBindingsAST
if' [unfix -> NBoolean cond, a, b] =
  if cond
    then a
    else b
if' x = error $ "expected bool then 2 args for if but got " ++ show x

eq' :: [NoBindingsAST] -> NoBindingsAST
eq' [a, b] = Fix . NBoolean $ a == b
eq' x = error $ "expected 2 args for == but got " ++ show x

intBinOp :: (Int -> Int -> Int) -> [NoBindingsAST] -> NoBindingsAST
intBinOp f [unfix -> NNumber a, unfix -> NNumber b] = Fix . NNumber $ f a b
intBinOp _ x = error $ "expected 2 ints for intBinOp but got " ++ show x

primitives :: Bindings SimpleAST
primitives =
  M.fromList . fmap (\x -> (x, Fix $ SBuiltin x [])) $
  ["+", "-", "*", "++", "if", "=="]
-- class Collapsable f where
--   collapse :: f -> SimpleAST
-- instance Collapsable (SimpleAST -> SimpleAST) where
--   collapse f = sfunc (\a -> f a)
-- instance Collapsable (SimpleAST -> SimpleAST -> SimpleAST) where
--   collapse f = sfunc (\a -> collapse $ f a)
-- instance Collapsable (SimpleAST -> SimpleAST -> SimpleAST -> SimpleAST) where
--   collapse f = sfunc (\a -> collapse $ f a)
-- intBinOp :: (Int -> Int -> Int) -> SimpleAST
-- intBinOp f = collapse go
--   where
--     go (unfix -> SNumber a) (unfix -> SNumber b) = Fix $ SNumber (f a b)
--     go x y = error $ "expected numbers in func but got: " ++ show (x, y)
-- -- stringBinOp :: (String -> String -> String) -> Prim
-- -- stringBinOp f = collapse go
--   -- where
--   --   go (Str a) (Str b) = Str (f a b)
--   --   go x y = error $ "expected strings in func, got: " ++ show (x, y)
-- -- ifPrim :: Prim -> Prim -> Prim -> Prim
-- -- ifPrim (Boolean True) a _ = a
-- -- ifPrim (Boolean False) _ b = b
-- -- ifPrim x _ _ = error $ "expected bool in 'if', got: " ++ show x
-- -- eq :: Prim -> Prim -> Prim
-- -- eq a b = Boolean (a == b)
-- -- singleBind :: Prim -> Prim -> Prim
-- -- singleBind (BindingSymbol name) val = BindingsPrim [(name, val)]
-- -- singleBind x _ = error $ "expected BindingSymbol in '=', got: " ++ show x
