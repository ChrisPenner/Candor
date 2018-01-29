module AST where

import RIO

data AST =
  Appl AST [AST]
  | Str String
  | Number Int
  | Symbol String
  | Binder String
  | Func [AST] AST
  | List [AST]
  | Builtin String
  | Bindings (Map String AST)
  deriving (Show, Eq)
