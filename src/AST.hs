module AST where

import RIO

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

type Bindings = Map String AST

type TypeBindings = Map String Type

data Type =
  TNumber
    | TString
    | TBool
    | TBinder
    | TList Type
    | TAny
    | TBindings TypeBindings
    | TFunc [Type]
  deriving (Show, Eq)

type EvalM a = ReaderT Bindings (Either String) a

