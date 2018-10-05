{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE FlexibleInstances #-}

module Primitives where

import AST

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
primitives :: Bindings Int
primitives = undefined
  -- M.fromList
  --   [ ("+", intBinOp (+))
  --   , ("-", intBinOp (-))
  --   , ("*", intBinOp (*))
  --   , ("++", stringBinOp (++))
  --   , ("if", collapse ifPrim)
  --   , ("==", collapse eq)
  --   , ("=", collapse singleBind)
  --   ]
-- class Collapsable f where
  -- collapse :: f -> Prim
-- instance Collapsable (Prim -> Prim) where
  -- collapse = Func
-- instance Collapsable (Prim -> Prim -> Prim) where
  -- collapse f = Func $ collapse . f
-- instance Collapsable (Prim -> Prim -> Prim -> Prim) where
  -- collapse f = Func $ collapse . f
-- intBinOp :: (Int -> Int -> Int) -> Prim
-- intBinOp f = collapse go
  -- where
  --   go (Number a) (Number b) = Number (f a b)
  --   go x y = error $ "expected numbers in func, got: " ++ show (x, y)
-- stringBinOp :: (String -> String -> String) -> Prim
-- stringBinOp f = collapse go
  -- where
  --   go (Str a) (Str b) = Str (f a b)
  --   go x y = error $ "expected strings in func, got: " ++ show (x, y)
-- ifPrim :: Prim -> Prim -> Prim -> Prim
-- ifPrim (Boolean True) a _ = a
-- ifPrim (Boolean False) _ b = b
-- ifPrim x _ _ = error $ "expected bool in 'if', got: " ++ show x
-- eq :: Prim -> Prim -> Prim
-- eq a b = Boolean (a == b)
-- singleBind :: Prim -> Prim -> Prim
-- singleBind (BindingSymbol name) val = BindingsPrim [(name, val)]
-- singleBind x _ = error $ "expected BindingSymbol in '=', got: " ++ show x
