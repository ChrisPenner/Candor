{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}

module AST where

import Data.Eq.Deriving
import Data.Functor.Foldable
import Data.List
import Data.List.NonEmpty as NE
import Data.Map as M
import RIO
import Text.Show.Deriving

class Pretty a where
  pretty :: a -> String

data ASTF r
  = Appl r
         [r]
  | Str String
  | Number Int
  | Boolean Bool
  | Symbol String
  | FuncDef (NonEmpty String)
            r
  | List [r]
  | Builtin String
  | Bindings (Map String r)
  deriving (Eq)

deriveShow1 ''ASTF

deriveEq1 ''ASTF

type AST = Fix ASTF

instance Pretty a => Pretty [a] where
  pretty xs = "[ " ++ foldMap ((++ ",") . pretty) xs ++ "]"

instance Pretty AST where
  pretty (Fix r) = pretty r

instance (Pretty r) => Pretty (ASTF r) where
  pretty (Appl f args) =
    "(" ++ pretty f ++ intercalate " " (pretty <$> args) ++ ")"
  pretty (Str s) = "\"" ++ s ++ "\""
  pretty (Number n) = show n
  pretty (Boolean b) =
    if b
      then "T"
      else "F"
  pretty (Symbol n) = n
  pretty (FuncDef args expr) =
    "{" ++ show (NE.toList args) ++ " -> " ++ pretty expr ++ "}"
  pretty (List args) = "[" ++ intercalate ", " (pretty <$> args) ++ "]"
  pretty (Builtin name) = name
  pretty (Bindings binds) =
    "{" ++ intercalate ", " (showBind <$> M.toList binds) ++ "}"
    where
      showBind (k, v) = k ++ ": " ++ pretty v

type Bindings = Map String AST

type EvalM a = ReaderT Bindings (Either String) a
