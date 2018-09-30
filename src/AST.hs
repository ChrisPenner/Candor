{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}

module AST where

import Data.Functor.Foldable
import Data.List
import Data.Map as M
import RIO
import Text.Show.Deriving

class Pretty a where
  pretty :: a -> String

type AST = Fix ASTF

data ASTF r
  = Appl r
         [r]
  | FuncDef String
            r
  | List [r]
  | Builtin String
  | Symbol String
  | P Prim
  | Bindings (Map String AST)
  | BindingsPrim (Map String Prim)
  deriving (Functor, Foldable, Typeable)

data Prim
  = Str String
  | Number Int
  | Boolean Bool
  | Func (Prim -> Prim)
  deriving (Typeable)

instance Show Prim where
  show (Str s) = "\"" ++ s ++ "\""
  show (Number n) = show n
  show (Boolean b) = show b
  show (Func _) = "Func (...)"

-- deriving instance Show Prim
-- deriving instance Eq Prim
-- 
deriveShow ''ASTF

deriveShow1 ''ASTF

instance Pretty a => Pretty [a] where
  pretty xs = "[ " ++ foldMap ((++ ",") . pretty) xs ++ "]"

instance Pretty (AST) where
  pretty (Fix r) = pretty r

instance Pretty Prim where
  pretty (Str s) = "\"" ++ s ++ "\""
  pretty (Number n) = show n
  pretty (Func _) = "Func (...)"
  pretty (Boolean b) =
    if b
      then "T"
      else "F"

instance (Pretty r) => Pretty (ASTF r) where
  pretty (Appl f args) =
    "(" ++ pretty f ++ intercalate " " (pretty <$> args) ++ ")"
  pretty (P p) = pretty p
  pretty (FuncDef arg expr) = "{" ++ show arg ++ " -> " ++ pretty expr ++ "}"
  pretty (List args) = "[" ++ intercalate ", " (pretty <$> args) ++ "]"
  pretty (Builtin name) = name
  pretty (Symbol name) = name
  pretty (Bindings binds) =
    "{" ++ intercalate ", " (showBind <$> M.toList binds) ++ "}"
    where
      showBind (k, v) = k ++ ": " ++ pretty v

-- tag :: Monoid a => Fix ASTF -> Cofree ASTF a
-- tag = cata (mempty :<)
prim :: Prim -> AST
prim = Fix . P

binding :: Map String Prim -> AST
binding = Fix . BindingsPrim

sym :: String -> AST
sym = Fix . Symbol

num :: Int -> AST
num = prim . Number

str :: String -> AST
str = prim . Str

boolean :: Bool -> AST
boolean = prim . Boolean

type Bindings = Map String Prim
