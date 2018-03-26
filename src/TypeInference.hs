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
import Data.List.NonEmpty as NE (NonEmpty(..), toList, fromList)
import Control.Monad.State
import Control.Monad.Except
import Data.Foldable
import Primitives
import Control.Lens hiding (List)

import AST
import Types

data TypeInfo = TypeInfo
  { _freshNames :: [String]
  , _subMap :: Substitutions
  }
makeLenses ''TypeInfo

type InferM a = ExceptT InferenceError (ReaderT Env (State TypeInfo)) a

subM :: Sub t => t -> InferM t
subM t = do
  subs <- use subMap
  return (sub subs t)

inferType :: AST -> Either InferenceError Monotype
inferType = runInference (Env primitiveTypes) . infer

freshVar :: InferM Monotype
freshVar = do
  (name:rest) <- use freshNames
  freshNames .= rest
  return (TVar name)

infNames :: [String]
infNames = ((:[]) <$> ['a'..'z']) ++ do
  n <- [1..] :: [Int]
  a <- ['a'..'z']
  return (a : show n)

runInference :: Env -> InferM a -> Either InferenceError a
runInference env = flip evalState typeInfo . flip runReaderT env . runExceptT
  where
    typeInfo = TypeInfo {_freshNames=infNames, _subMap=mempty}

class Unifyable a b where
  unify :: a -> b -> InferM Monotype

instance Unifyable Monotype Monotype where
  unify :: Monotype -> Monotype -> InferM Monotype
  unify (TVar a) b = bindVar a b
  unify a (TVar b) = bindVar b a
  unify (TConst a) (TConst b) | a == b = return (TConst a)
  unify (TList a) (TList b) = TList <$> unify a b
  unify (TBindings env1) (TBindings env2) = unify env1 env2
  unify (TFunc a b) (TFunc a' b') = do
    fType <- unify a a'
    returnType <- (subM (b, b') >>= uncurry unify)
    return (TFunc fType returnType)
  unify a b = throwError (CannotUnify a b)

instance Unifyable Env Env where
  unify (Env a) (Env b) = do
    combinedEnvs <- sequenceA $ M.fromSet pairs allKeys
    return (TBindings (Env combinedEnvs))
    where
      allKeys = M.keysSet a <> M.keysSet b
      pairs k = Forall [] <$>
        case (M.lookup k a, M.lookup k b) of
          (Just (Forall _ a), Just (Forall _ b)) -> unify a b
          (Just (Forall _ a), Nothing) -> return a
          (Nothing, Just (Forall _ b)) -> return b
          (Nothing, Nothing) -> error $ "somehow found empty pair in env unification!"



bindVar :: String -> Monotype -> InferM Monotype
bindVar name (TVar t) | name == t = return $ TVar t
bindVar name t | name `S.member` getFree t =
  throwError $ OccursCheckFailed name t -- Can't bind a type var to a definition containing the same type var: infinite recursion
bindVar name t = do
  subMap <>= Substitutions [(name, t)]
  return t

extendEnv :: Env -> String -> Polytype -> Env
extendEnv (Env m) name t = Env $ M.insert name t m

infer :: AST -> InferM Monotype
infer ast =
  case ast of
    Str{} -> return stringT
    Number{} -> return intT
    Boolean{} -> return boolT
    Symbol name -> inferSymbol name
    Builtin name -> inferSymbol name
    FuncDef args expr -> inferFunc args expr
    List l -> inferList l
    Bindings b ->
      TBindings . Env . fmap (Forall []) <$> traverse infer b
    Appl f args -> inferAppl f args

inferAppl :: AST -> [AST] -> InferM Monotype
inferAppl f args = do
  fType <- infer f
  case (fType, args) of
    (TBindings env, [expr]) -> local (<> env) (infer expr)
    _ -> do
      argTypes <- traverse infer args
      foldM go fType argTypes >>= subM
    where
      go fType next =
        subM next >>= applType fType

applType :: Monotype -> Monotype -> InferM Monotype
applType (TFunc accept returnType) arg = do
  _ <- unify accept arg
  subM returnType
applType ast _ = error $ "expected TFunc but got: " ++ show ast


inferFunc :: NonEmpty String -> AST -> InferM Monotype
inferFunc (NE.toList -> args) expr = do
  argTypes <- replicateM (length args) freshVar
  let argEnv = Env . M.fromList $ zip args (Forall mempty <$> argTypes)
  returnType <- local (<> argEnv) $ infer expr
  subM $ nestFuncs (NE.fromList argTypes) returnType
    where
      mapToFresh name = freshVar >>= return . (name,)

nestFuncs :: NonEmpty Monotype -> Monotype -> Monotype
nestFuncs (x:|[]) returnType = 
  TFunc x returnType
nestFuncs (x:|(y:ys)) returnType = 
  TFunc x (nestFuncs (y:|ys) returnType)

inferList :: [AST] -> InferM Monotype
inferList [] = TList <$> freshVar
inferList asts = do
  (nub -> (t:ts)) <- traverse infer asts
  TList <$> foldlM unify t ts

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
        let freshSubstActions = M.fromSet (const freshVar) xs
        freshSubsts <- sequenceA freshSubstActions
        return . Substitutions $ freshSubsts
