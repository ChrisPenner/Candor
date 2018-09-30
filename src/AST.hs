{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}

module AST where

import Data.Eq.Deriving
import Data.Functor.Foldable
import Data.List
import qualified Data.Map as M
import RIO
import Text.Show.Deriving

class Pretty a where
  pretty :: a -> String

type AST = Fix ASTF

data ASTF r
  = Appl r
         r
  | FuncDef String
            r
  | List [r]
  | Symbol String
  | P Prim
  | Bindings (Map String AST)
  deriving (Functor, Foldable, Typeable)

data Prim
  = Str String
  | Number Int
  | Boolean Bool
  | Func (Prim -> Prim)
  | BindingsPrim (Map String Prim)
  | BindingSymbol String
  deriving (Typeable)

instance Show Prim where
  show (Str s) = "(Str " ++ s ++ ")"
  show (Number n) = "(Number " ++ show n ++ ")"
  show (Boolean b) = "(Boolean " ++ show b ++ ")"
  show (Func _) = "(Func ...)"
  show (BindingsPrim m) = "(BindingsPrim " ++ show m ++ ")"
  show (BindingSymbol s) = "(BindingSymbol " ++ show s ++ ")"

instance Eq Prim where
  Str s == Str s' = s == s'
  Number n == Number n' = n == n'
  Boolean b == Boolean b' = b == b'
  BindingSymbol s == BindingSymbol s' = s == s'
  (Func _) == _ = False
  _ == _ = False

deriveShow ''ASTF

deriveShow1 ''ASTF

deriveEq ''ASTF

deriveEq1 ''ASTF

instance Pretty a => Pretty [a] where
  pretty xs = "[ " ++ foldMap ((++ ",") . pretty) xs ++ "]"

instance Pretty (AST) where
  pretty (Fix r) = pretty r

instance Pretty Prim where
  pretty (Str s) = "\"" ++ s ++ "\""
  pretty (Number n) = show n
  pretty (Func _) = "Func (...)"
  pretty (BindingsPrim m) = "BindingsPrim{" ++ show (pretty <$> m) ++ "}"
  pretty (BindingSymbol s) = "BindingSymbol(" ++ s ++ ")"
  pretty (Boolean b) =
    if b
      then "T"
      else "F"

instance (Pretty r) => Pretty (ASTF r) where
  pretty (Appl f arg) = "(" ++ pretty f ++ pretty arg ++ ")"
  pretty (P p) = pretty p
  pretty (FuncDef arg expr) = "{" ++ show arg ++ " -> " ++ pretty expr ++ "}"
  pretty (List args) = "[" ++ intercalate ", " (pretty <$> args) ++ "]"
  pretty (Symbol name) = name ++ " "
  pretty (Bindings binds) =
    "{" ++ intercalate ", " (showBind <$> M.toList binds) ++ "}"
    where
      showBind (k, v) = k ++ ": " ++ pretty v

-- tag :: Monoid a => Fix ASTF -> Cofree ASTF a
-- tag = cata (mempty :<)
prim :: Prim -> AST
prim = Fix . P

apply :: AST -> AST -> AST
apply f arg = Fix (Appl f arg)

curryApply :: AST -> [AST] -> AST
curryApply f args = foldl' go f args
  where
    go :: AST -> AST -> AST
    go f' arg = Fix (Appl f' arg)

funcDef :: String -> AST -> AST
funcDef arg expr = Fix (FuncDef arg expr)

binding :: Map String AST -> AST
binding = Fix . Bindings

sym :: String -> AST
sym = Fix . Symbol

num :: Int -> AST
num = prim . Number

str :: String -> AST
str = prim . Str

boolean :: Bool -> AST
boolean = prim . Boolean

bindSym :: String -> AST
bindSym = prim . BindingSymbol

type Bindings = Map String Prim
