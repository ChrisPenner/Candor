module AST where

import RIO

data AST =
  Appl AST [AST]
  | Str String
  | Number Int
  | Boolean Bool
  | Symbol String
  | Binder String
  | FuncDef [AST] AST
  | List [AST]
  | Builtin String
  | Bindings (Map String AST)
  deriving (Show, Eq)
