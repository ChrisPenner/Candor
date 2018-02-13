module AST where

import RIO

import Types

type Bindings = Map String AST

data AST =
  Appl AST [AST]
  | Str String
  | Number Int
  | Boolean Bool
  | Symbol String
  | Binder String
  | FuncDef [String] AST
  | List [AST]
  | Builtin Type String
  | Bindings (Map String AST)
  deriving (Show, Eq)
