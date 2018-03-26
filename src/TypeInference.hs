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

freshenAll :: (Sub t, HasFreeTypes t) => t -> InferM (Substitutions, t)
freshenAll t = do
  freshMap <- sequenceA . M.fromSet (const freshName) $ getFree t
  let subs = (Substitutions (TVar <$> freshMap))
  return $ (subs, sub subs t)

subM :: Sub t => t -> InferM t
subM t = do
  subs <- use subMap
  return (sub subs t)

inferType :: AST -> Either InferenceError Monotype
inferType = runInference (Env primitiveTypes) . infer

freshName :: InferM String
freshName = do
  (name:rest) <- use freshNames
  freshNames .= rest
  return name

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
  unify (TFunc a b) (TFunc a' b') = do
    fType <- unify a a'
    returnType <- (subM (b, b') >>= uncurry unify)
    return (TFunc fType returnType)
  unify a b = throwError (CannotUnify a b)

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
    Bindings{} -> return bindingsT
    Appl f args -> inferAppl f args

inferAppl :: AST -> [AST] -> InferM Monotype
inferAppl f args = do
  fType <- infer f
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
inferFunc args expr = do
  let env' = Env . M.fromList . fmap toKeyVal . NE.toList $ args
  returnType <- local (<> env') $ infer expr
  argTypes <- traverse (subM . TVar) args
  returnType' <- subM returnType
  return (nestFuncs argTypes returnType')
    where
      toKeyVal name = (name, Forall mempty (TVar name))

nestFuncs :: NonEmpty Monotype -> Monotype -> Monotype
nestFuncs (x:|[]) returnType = TFunc x returnType
nestFuncs (x:|(y:ys)) returnType = TFunc x (nestFuncs (y:|ys) returnType)

inferList :: [AST] -> InferM Monotype
inferList [] = TList . TVar <$> freshName
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
        let freshSubstActions = M.fromSet (const freshName) xs
        freshSubsts <- sequenceA freshSubstActions
        return . Substitutions $ (TVar <$> freshSubsts)
