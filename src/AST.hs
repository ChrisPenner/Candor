module AST where

import RIO
import Data.List.NonEmpty

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

type Bindings = Map String AST

type EvalM a = ReaderT Bindings (Either String) a
