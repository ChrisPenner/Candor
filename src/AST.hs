module AST where

import RIO
import Data.List.NonEmpty as NE
import Data.List
import Data.Map as M

class Pretty a where
  pretty :: a -> String

data AST =
  Appl AST [AST]
  | Str String
  | Number Int
  | Boolean Bool
  | Symbol String
  | FuncDef (NonEmpty String) AST
  | List [AST]
  | Builtin String
  | Bindings (Map String AST)
  deriving (Show, Eq)

instance Pretty AST where
  pretty (Appl f args) = "(" ++ pretty f ++ intercalate " " (pretty <$> args) ++ ")"
  pretty (Str s) = "\"" ++ s ++ "\""
  pretty (Number n) = show n
  pretty (Boolean b) = if b then "T" else "F"
  pretty (Symbol n) = n
  pretty (FuncDef args expr) = "{" ++ show (NE.toList args) ++ " -> " ++ pretty expr ++ "}"
  pretty (List args) = "[" ++ intercalate ", " (pretty <$> args) ++ "]"
  pretty (Builtin name) = name
  pretty (Bindings binds) = "{" ++ intercalate ", " (showBind <$> M.toList binds) ++ "}"
    where
      showBind (k, v) = k ++ ": " ++ pretty v

type Bindings = Map String AST

type EvalM a = ReaderT Bindings (Either String) a
