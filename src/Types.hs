module Types where

import RIO
import Data.Set as S
import Data.List as L
import AST

data TypeConst = IntT | StringT | BoolT | BinderT | BindingsT
  deriving (Show, Eq)

instance Pretty TypeConst where
  pretty IntT = "Int"
  pretty StringT = "String"
  pretty BoolT = "Bool"
  pretty BinderT = "Binder"
  pretty BindingsT = "Bindings"

intT, stringT, boolT, binderT, bindingsT, varT :: Monotype
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
    deriving (Show, Eq)

instance Pretty Monotype where
  pretty (TVar s) = s
  pretty (TConst s) = pretty s
  pretty (TFunc a b) = "(" <> pretty a <> " -> " <> pretty b <> ")"
  pretty (TList m) = "[" <> pretty m <> "]"

type FreeTypes = S.Set String

data Polytype = Forall FreeTypes Monotype
  deriving Show

instance Pretty Polytype where
  pretty (Forall quantifieds m) =
    "∀ " <> (L.intercalate " " $ S.toList quantifieds) <> ". " <> pretty m

