{-# language OverloadedLists #-}
{-# language InstanceSigs #-}
{-# language MultiParamTypeClasses #-}
{-# language TypeFamilies #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language ViewPatterns #-}
module TypeInference where

import AST
import GHC.Exts

import RIO
import qualified Data.Set as S
import qualified Data.Map as M
import Data.List as L
import Data.Foldable
import Control.Monad.State
import Control.Monad.Except
import qualified Data.Bifunctor as Bi

type FreeTypes = S.Set String
type InferM a = ExceptT InferenceError (State [String]) a

newtype Env = Env (M.Map String Polytype)
  deriving (Show, Monoid)

instance IsList Env where
  type Item Env = (String, Polytype)
  fromList = Env . M.fromList

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



freshName :: InferM String
freshName = do
  (name:rest) <- get
  put rest
  return name


data InferenceError =
      CannotUnify Monotype Monotype
    | OccursCheckFailed String Monotype
    | UnknownIdentifier String
    deriving (Eq)

instance Show InferenceError where
  show (CannotUnify t1 t2) = "Cannot unify " <> show t1 <> " with " <> show t2
  show (OccursCheckFailed name ty) =
    "Occurs check failed: " <> show name <> " already appears in " <> show ty
  show (UnknownIdentifier name) = "Unknown identifier: " <> show name

data Polytype = Forall FreeTypes Monotype

instance Show Polytype where
  show (Forall quantifieds m) =
    "∀ " <> (L.intercalate " " $ S.toList quantifieds) <> ". " <> show m

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


infNames :: [String]
infNames = ((:[]) <$> ['a'..'z']) ++ do
  n <- [1..] :: [Int]
  a <- ['a'..'z']
  return (a : show n)

runInference :: InferM a -> Either InferenceError a
runInference = flip evalState infNames . runExceptT

class Unifyable a b where
  unify :: a -> b -> InferM Substitutions

instance Unifyable Monotype Monotype where
  unify :: Monotype -> Monotype -> InferM Substitutions
  unify (TVar a) b = bindVar a b
  unify a (TVar b) = bindVar b a
  unify (TConst a) (TConst b) | a == b = return mempty
  unify (TList a) (TList b) = unify a b
  unify (TFunc a b) (TFunc a' b') = do
    subs <- unify a a'
    unify (sub subs b) b'
  unify a b = throwError (CannotUnify a b)

bindVar :: String -> Monotype -> InferM Substitutions
bindVar name (TVar t) | name == t = pure mempty
bindVar name t | name `S.member` getFree t = 
  throwError $ OccursCheckFailed name t -- Can't bind a type var to a definition containing the same type var: infinite recursion
bindVar name t = return $ Substitutions [(name, t)]

extendEnv :: Env -> String -> Polytype -> Env
extendEnv (Env m) name t = Env $ M.insert name t m

infer :: Env -> AST -> InferM (Substitutions, Monotype)
infer env ast =
  case ast of
    Str{} -> return (mempty, stringT)
    Number{} -> return (mempty, intT)
    Boolean{} -> return (mempty, boolT)
    Symbol name -> inferSymbol env name 
    Binder{} -> return (mempty, binderT)
    FuncDef args expr -> inferFunc env args expr
    List l -> inferList env l
    Builtin t _ -> return (mempty, t)
    Bindings{} -> return (mempty, bindingsT)
    Appl f args -> do
      inferAppl env f args

inferAppl :: Env -> AST -> [AST] -> InferM (Substitutions, Monotype)
inferAppl env f args = do
  (subs, fType) <- infer env f
  (subs', argTypes) <- unzip <$> traverse (infer env) args
  foldM go (mempty, fType) argTypes
    where
      go (subs, fType) next = applType fType (sub subs next)

applType :: Monotype -> Monotype -> InferM (Substitutions, Monotype)
applType (TFunc accept returnType) arg = do
  subs <- unify accept arg
  return (subs, sub subs returnType)


inferFunc :: Env -> [String] -> AST -> InferM (Substitutions, Monotype)
inferFunc env args expr = do
  let argSet = S.fromList args
      env' = Env $ M.fromSet (Forall mempty . TVar) argSet
  (subs, returnType) <- infer (env  <> env') expr
  let argTypes = sub subs . TVar <$> args
  return (subs, nestFuncs argTypes (sub subs returnType))

nestFuncs :: [Monotype] -> Monotype -> Monotype
nestFuncs (x:[]) returnType = TFunc x returnType
nestFuncs (x:xs) returnType = TFunc x (nestFuncs xs returnType)

inferList :: Env -> [AST] -> InferM (Substitutions, Monotype)
inferList env [] = do
  fresh <- freshName
  return (mempty, TList (TVar fresh))
inferList env xs = do
  (subs, (t:ts)) <- unzip <$> traverse (infer env) xs
  subs' <- sequenceA $ zipWith unify (t:ts) ts
  return (fold (subs <> subs'), TList t)


inferSymbol :: Env -> String -> InferM (Substitutions, Monotype)
inferSymbol env name = do
  symbolType <- lookupSymbol env name
  boundMonotype <- freshNameAll symbolType
  return (mempty, boundMonotype)

lookupSymbol :: Env -> String -> InferM Polytype
lookupSymbol (Env env) name = 
  case M.lookup name env of
    Just x  -> return x
    Nothing -> throwError (UnknownIdentifier name)

freshNameAll :: Polytype -> InferM Monotype
freshNameAll (Forall qs t) = do
    mapping <- substituteAllWithFresh qs
    pure (sub mapping t)
  where
    -- For each given name, add a substitution from that name to a fresh type
    -- variable to the result.
    substituteAllWithFresh :: Set String -> InferM Substitutions
    substituteAllWithFresh xs = do
        let freshSubstActions = M.fromSet (const freshName) xs
        freshSubsts <- sequenceA freshSubstActions
        return . Substitutions $ (TVar <$> freshSubsts)
