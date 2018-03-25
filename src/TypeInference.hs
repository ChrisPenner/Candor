{-# language TemplateHaskell #-}
{-# language OverloadedLists #-}
{-# language InstanceSigs #-}
{-# language MultiParamTypeClasses #-}
{-# language TypeFamilies #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language ViewPatterns #-}
{-# language TupleSections #-}
module TypeInference where

import RIO
import qualified Data.Set as S
import qualified Data.Map as M
import Data.List as L
import Data.List.NonEmpty as NE (NonEmpty(..), toList)
import Control.Monad.State
import Control.Monad.Except
import Primitives
import Control.Lens hiding (List)

import AST
import Types

data TypeInfo = TypeInfo
  { _freshNames :: [String]
  , _subMap :: Substitutions
  }
makeLenses ''TypeInfo

freshenAll :: (Sub t, HasFreeTypes t) => t -> InferM (Substitutions, t)
freshenAll t = do
  freshMap <- sequenceA . M.fromSet (const freshName) $ getFree t
  let subs = (Substitutions (TVar <$> freshMap))
  return $ (subs, sub subs t)

inferType :: AST -> Either InferenceError Monotype
inferType = fmap snd . runInference (Env primitiveTypes) . infer

type InferM a = ExceptT InferenceError (ReaderT Env (State [String])) a

freshName :: InferM String
freshName = do
  (name:rest) <- get
  put rest
  return name

infNames :: [String]
infNames = ((:[]) <$> ['a'..'z']) ++ do
  n <- [1..] :: [Int]
  a <- ['a'..'z']
  return (a : show n)

runInference :: Env -> InferM a -> Either InferenceError a
runInference env = flip evalState infNames . flip runReaderT env . runExceptT

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

infer :: AST -> InferM (Substitutions, Monotype)
infer ast =
  case ast of
    Str{} -> return (mempty, stringT)
    Number{} -> return (mempty, intT)
    Boolean{} -> return (mempty, boolT)
    Symbol name -> (mempty,) <$> inferSymbol name
    Builtin name -> (mempty,) <$> inferSymbol name
    FuncDef args expr -> inferFunc args expr
    List l -> inferList l
    Bindings{} -> return (mempty, bindingsT)
    Appl f args -> do
      inferAppl f args

inferAppl :: AST -> [AST] -> InferM (Substitutions, Monotype)
inferAppl f args = do
  (subsA, fType) <- infer f
  (subsB, argTypes) <- unzip <$> traverse infer args
  (subsC, resultT) <- foldM go (mempty, fType) argTypes
  return (subsA <> fold subsB <> subsC, resultT)
    where
      go (subs, fType) next = do
        (newSubs, t) <- applType fType (sub subs next)
        return (subs <> newSubs, t)

applType :: Monotype -> Monotype -> InferM (Substitutions, Monotype)
applType (TFunc accept returnType) arg = do
  subs <- unify accept arg
  return (subs, sub subs returnType)
applType ast _ = error $ "expected TFunc but got: " ++ show ast


inferFunc :: NonEmpty String -> AST -> InferM (Substitutions, Monotype)
inferFunc args expr = do
  let env' = Env . M.fromList . fmap toKeyVal . NE.toList $ args
  (subs, returnType) <- local (<> env') $ infer expr
  let argTypes = sub subs . TVar <$> args
  return (subs, nestFuncs argTypes (sub subs returnType))
    where
      toKeyVal name = (name, Forall mempty (TVar name))

nestFuncs :: NonEmpty Monotype -> Monotype -> Monotype
nestFuncs (x:|[]) returnType = TFunc x returnType
nestFuncs (x:|(y:ys)) returnType = TFunc x (nestFuncs (y:|ys) returnType)

inferList :: [AST] -> InferM (Substitutions, Monotype)
inferList [] = do
  fresh <- freshName
  return (mempty, TList (TVar fresh))
inferList xs = do
  (subs, (t:ts)) <- unzip <$> traverse infer xs
  subs' <- sequenceA $ zipWith unify (t:ts) ts
  return (fold (subs <> subs'), TList t)


inferSymbol :: String -> InferM Monotype
inferSymbol name = do
  symbolType <- lookupSymbol name
  boundMonotype <- freshNameAll symbolType
  return boundMonotype

lookupSymbol :: String -> InferM Polytype
lookupSymbol name = do
  Env env <- ask
  case M.lookup name env of
    Just x  -> return x
    Nothing -> throwError (UnknownIdentifier name)

freshNameAll :: Polytype -> InferM Monotype
freshNameAll (Forall qs t) = do
    freshMapping <- substituteAllWithFresh qs
    pure (sub freshMapping t)
  where
    -- For each given name, add a substitution from that name to a fresh type
    -- variable to the result.
    substituteAllWithFresh :: Set String -> InferM Substitutions
    substituteAllWithFresh xs = do
        let freshSubstActions = M.fromSet (const freshName) xs
        freshSubsts <- sequenceA freshSubstActions
        return . Substitutions $ (TVar <$> freshSubsts)
