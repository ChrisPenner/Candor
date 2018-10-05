{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}

module AST where

-- import Data.Eq.Deriving
import Data.Functor.Foldable
import Data.List
import qualified Data.Map as M
import RIO

-- import Text.Show.Deriving
class Pretty a where
  pretty :: a -> String

type AST = Fix ASTF

data ASTF r
    -- Appl r
         -- r
  = List [r]
  | Symbol String
  | BindingSymbol String
  | FuncDef (AST -> r)
  | Str String
  | Number Int
  | Boolean Bool
  | Binding String
            r
  deriving (Functor, Typeable)

instance Show r => Show (ASTF r) where
  show (Str s) = "(Str " ++ s ++ ")"
  show (Number n) = "(Number " ++ show n ++ ")"
  show (Boolean b) = "(Boolean " ++ show b ++ ")"
  show (Binding name val) = "(Binding " ++ name ++ " " ++ show val ++ ")"
  show (FuncDef _) = "(FuncDef ...)"
  -- show (Appl f arg) = "(Appl " ++ show f ++ " " ++ show arg ++ ")"

-- data Prim
-- instance Show Prim where
--   show (Str s) = "(Str " ++ s ++ ")"
--   show (Number n) = "(Number " ++ show n ++ ")"
--   show (Boolean b) = "(Boolean " ++ show b ++ ")"
--   show (Bindings b) = "(Bindings " ++ show b ++ ")"
--   show (FuncDef n ast) = "(FuncDef " ++ show b ++ ")"
-- deriveShow ''ASTF
-- deriveShow1 ''ASTF
-- deriveEq ''Prim
-- deriveShow ''Prim
instance Pretty a => Pretty [a] where
  pretty xs = "[ " ++ foldMap ((++ ",") . pretty) xs ++ "]"

instance Pretty (AST) where
  pretty (Fix r) = pretty r

-- instance Pretty Prim where
--   pretty (Str s) = "\"" ++ s ++ "\""
--   pretty (Number n) = show n
--   pretty (FuncDef arg expr) = "{" ++ show arg ++ " -> " ++ pretty expr ++ "}"
--   pretty (Bindings m) =
--     "{" ++
--     M.foldrWithKey (\k v r -> k ++ ": " ++ pretty v ++ ", " ++ r) "" m ++ "}"
--   pretty (Boolean b) =
--     if b
--       then "T"
--       else "F"
instance (Pretty r) => Pretty (ASTF r)
  -- pretty (Appl f arg) = "(" ++ pretty f ++ pretty arg ++ ")"
                                                                where
  pretty (List args) = "[" ++ intercalate ", " (pretty <$> args) ++ "]"
  pretty (Symbol name) = name ++ " "
  pretty (BindingSymbol name) = ":" ++ name ++ " "
  pretty (Str s) = "\"" ++ s ++ "\""
  pretty (Number n) = show n
  -- pretty (FuncDef arg expr) = "{" ++ show arg ++ " -> " ++ pretty expr ++ "}"
  -- pretty (Bindings m) =
  --   "{" ++
  --   M.foldrWithKey (\k v r -> k ++ ": " ++ pretty v ++ ", " ++ r) "" m ++ "}"
  -- pretty (Boolean b) =
  --   if b
  --     then "T"
  --     else "F"
  -- where
    -- go :: AST -> AST -> AST
    -- go f' arg = Fix (Appl f' arg)

-- -- tag :: Monoid a => Fix ASTF -> Cofree ASTF a
-- -- tag = cata (mempty :<)
-- prim :: Prim -> AST
-- prim = Fix . P
-- apply :: AST -> AST -> AST
-- apply f arg = Fix (Appl f arg)
-- curryApply :: AST -> [AST] -> AST
-- curryApply f args = foldl' go f args
-- funcDef :: String -> AST -> AST
-- funcDef arg expr = Fix $ (FuncDef arg expr)
sym :: String -> AST
sym = Fix . Symbol

num :: Int -> AST
num = Fix . Number

str :: String -> AST
str = Fix . Str

boolean :: Bool -> AST
boolean = Fix . Boolean

bindSym :: String -> AST
bindSym = Fix . BindingSymbol

type Bindings r = Map String r
