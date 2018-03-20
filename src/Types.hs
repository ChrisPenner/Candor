module Types where

import RIO
import Data.Set as S
import Data.List as L

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

type FreeTypes = S.Set String
data Polytype = Forall FreeTypes Monotype

instance Show Polytype where
  show (Forall quantifieds m) =
    "∀ " <> (L.intercalate " " $ S.toList quantifieds) <> ". " <> show m

