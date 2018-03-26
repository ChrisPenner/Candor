{-# language InstanceSigs #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language TypeFamilies #-}
module Types where

import RIO
import Data.Set as S
import Data.List as L
import AST
import GHC.Exts (IsList(..))
import qualified Data.Map as M

data TypeConst = IntT | StringT | BoolT | BinderT
  deriving (Show, Eq)

instance Pretty TypeConst where
  pretty IntT = "Int"
  pretty StringT = "String"
  pretty BoolT = "Bool"
  pretty BinderT = "Binder"

intT, stringT, boolT, binderT, varT :: Monotype
intT = TConst IntT
stringT = TConst StringT
boolT = TConst BoolT
binderT = TConst BinderT
varT = TVar "a"

data Monotype =
  TVar String
    | TConst TypeConst  -- Things like Int, (), etc
    | TFunc Monotype Monotype
    | TList Monotype -- List of types
    | TBindings Env
    deriving (Show, Eq)

instance Pretty Monotype where
  pretty (TVar s) = s
  pretty (TConst s) = pretty s
  pretty (TFunc a b) = "(" <> pretty a <> " -> " <> pretty b <> ")"
  pretty (TList m) = "[" <> pretty m <> "]"
  pretty (TBindings env) = "Bindings: " ++ pretty env

type FreeTypes = S.Set String

data Polytype = Forall FreeTypes Monotype
  deriving (Show, Eq)

instance Pretty Polytype where
  pretty (Forall quantifieds m) =
    "∀ " <> (L.intercalate " " $ S.toList quantifieds) <> ". " <> pretty m

data InferenceError =
      CannotUnify Monotype Monotype
    | OccursCheckFailed String Monotype
    | UnknownIdentifier String
    deriving (Show, Eq)

instance Pretty InferenceError where
  pretty (CannotUnify t1 t2) = "Cannot unify " <> pretty t1 <> " with " <> pretty t2
  pretty (OccursCheckFailed name ty) =
    "Occurs check failed: " <> name <> " already appears in " <> pretty ty
  pretty (UnknownIdentifier name) = "Unknown identifier: " <> name

class HasFreeTypes t where
  getFree :: t -> FreeTypes

instance HasFreeTypes Monotype where
  getFree :: Monotype -> FreeTypes
  getFree (TVar s) = S.singleton s
  getFree (TConst _) = S.empty
  getFree (TFunc a b) = getFree a <> getFree b
  getFree (TList t) = getFree t

instance HasFreeTypes Polytype where
  getFree :: Polytype -> FreeTypes
  getFree (Forall quantifieds m) = getFree m S.\\ quantifieds

instance HasFreeTypes Env where
  getFree :: Env -> FreeTypes
  getFree (Env env) = foldMap getFree env

class Sub t where
  sub :: Substitutions -> t -> t

instance Sub Substitutions where
  sub s (Substitutions target) = Substitutions (fmap (sub s) target)

instance Sub Monotype where
  sub :: Substitutions -> Monotype -> Monotype
  sub (Substitutions subst) tvar@(TVar t) = M.findWithDefault tvar t subst
  sub _ t@(TConst _) = t
  sub subst (TFunc a b) = TFunc (sub subst a) (sub subst b)
  sub subst (TList a) = TList (sub subst a)

instance Sub Polytype where
  sub :: Substitutions -> Polytype -> Polytype
  sub subst@(Substitutions s) (Forall free t) = Forall leftOver (sub subst t)
    where
      leftOver = free S.\\ M.keysSet s

instance Sub Env where
  sub subst (Env env) = Env $ fmap (sub subst) env

instance (Sub a, Sub b) => Sub (a, b) where
  sub subst (a, b) = (sub subst a, sub subst b)

newtype Env = Env (M.Map String Polytype)
  deriving (Show, Monoid, Eq)

instance Pretty Env where
  pretty (Env binds) = "{" ++ intercalate ", " (showBind <$> M.toList binds) ++ "}"
    where
      showBind (k, v) = k ++ ": " ++ pretty v

instance IsList Env where
  type Item Env = (String, Polytype)
  fromList = Env . M.fromList
  toList (Env e) = M.toList e

newtype Substitutions = Substitutions (M.Map String Monotype)
  deriving (Show, Eq)

instance Monoid Substitutions where
  mappend subst1 subst2 = Substitutions (s1 `M.union` s2)
    where
      Substitutions s1 = subst1
      Substitutions s2 = sub subst1 subst2

  mempty = Substitutions mempty

instance IsList Substitutions where
  type Item Substitutions = (String, Monotype)
  fromList = Substitutions . M.fromList
  toList (Substitutions s) = M.toList s
