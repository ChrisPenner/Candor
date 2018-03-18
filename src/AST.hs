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
  | Builtin Monotype String
  | Bindings (Map String AST)
  deriving (Show, Eq)

type Bindings = Map String AST

type EvalM a = ReaderT Bindings (Either String) a

data TypeConst = IntT | StringT | BoolT | BinderT | BindingsT
  deriving (Show, Eq)

intT, stringT, boolT, binderT, bindingsT :: Monotype
intT = TConst IntT
stringT = TConst StringT
boolT = TConst BoolT
binderT = TConst BinderT
bindingsT = TConst BindingsT
varT = TVar "a"

data Monotype =
  TVar String
    | TConst TypeConst  -- Things like Int, (), etc
    | TFunc Monotype Monotype
    | TList Monotype -- List of types
    deriving Eq

instance Show Monotype where
  show (TVar s) = s
  show (TConst s) = show s
  show (TFunc a b) = "(" <> show a <> " -> " <> show b <> ")"
  show (TList m) = "[" <> show m <> "]"
