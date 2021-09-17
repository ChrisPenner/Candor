{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TypeInference where

import RIO
import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.Map.Merge.Lazy as M
import qualified Data.List.NonEmpty as NE
import Control.Monad.State
import Control.Monad.Except
import Data.Foldable
import Primitives
import Control.Lens hiding (List, (:>))
import qualified Data.Stream.Infinite as Stream
import Data.Stream.Infinite (Stream(..))

import AST
import Types

data TypeInfo = TypeInfo
  { _freshNames :: Stream String
  , _subMap :: Substitutions
  }

makeLenses ''TypeInfo

type InferM a = ExceptT InferenceError (ReaderT Env (State TypeInfo)) a

subM
  :: Sub t
  => t -> InferM t
subM t = do
  subs <- use subMap
  return (sub subs t)

inferType :: AST -> Either InferenceError Monotype
inferType = runInference (Env primitiveTypes) . infer

freshVar :: InferM Monotype
freshVar = do
  (name:>rest) <- use freshNames
  freshNames .= rest
  return (TVar name)

infNames :: Stream String
infNames =
    let firstNames :: NonEmpty String = pure <$> ('a' :| ['b' .. 'z'])
        otherNames = Stream.concat $ Stream.unfold (\n -> (liftA2 (:) ['a'..'z'] (pure $ show n), n)) (1 :: Int)
     in Stream.prepend firstNames otherNames

runInference :: Env -> InferM a -> Either InferenceError a
runInference env = flip evalState typeInfo . flip runReaderT env . runExceptT
  where
    typeInfo =
      TypeInfo
      { _freshNames = infNames
      , _subMap = mempty
      }

class Unifyable a b  where
  unify :: a -> b -> InferM Monotype

instance Unifyable (M.Map String Monotype) (M.Map String Monotype) where
  unify m n =
    TRows <$>
    M.mergeA M.preserveMissing M.preserveMissing unificationStrategy m n
    where
      unificationStrategy = M.zipWithAMatched (const unify)

instance Unifyable Monotype Monotype where
  unify :: Monotype -> Monotype -> InferM Monotype
  unify (TVar a) b = bindVar a b
  unify a (TVar b) = bindVar b a
  unify (TConst a) (TConst b)
    | a == b = return (TConst a)
  unify (TList a) (TList b) = TList <$> unify a b
  unify (TRows rows1) (TRows rows2) = unify rows1 rows2
  unify (TFunc a b) (TFunc a' b') = do
    fType <- unify a a'
    returnType <- (subM (b, b') >>= uncurry unify)
    return (TFunc fType returnType)
  unify a b = throwError (CannotUnify a b)

-- instance Unifyable Env Env where
--   unify (Env a) (Env b) = do
--     combinedEnvs <- sequenceA $ M.fromSet pairs allKeys
--     return combinedEnvs
--     where
--       allKeys = M.keysSet a <> M.keysSet b
--       pairs k =
--         Forall [] <$>
--         case (M.lookup k a, M.lookup k b) of
--           (Just (Forall _ a), Just (Forall _ b)) -> unify a b
--           (Just (Forall _ a), Nothing) -> return a
--           (Nothing, Just (Forall _ b)) -> return b
--           (Nothing, Nothing) ->
--             error $ "somehow found empty pair in env unification!"
bindVar :: String -> Monotype -> InferM Monotype
bindVar name (TVar t)
  | name == t = return $ TVar t
bindVar name t
  | name `S.member` getFree t = throwError $ OccursCheckFailed name t -- Can't bind a type var to a definition containing the same type var: infinite recursion
bindVar name t = do
  subMap <>= Substitutions (M.singleton name t)
  return t

extendEnv :: Env -> String -> Monotype -> Env
extendEnv (Env m) name t = Env $ M.insert name t m

infer :: AST -> InferM Monotype
infer ast =
  case ast of
    Str {} -> return stringT
    Number {} -> return intT
    Boolean {} -> return boolT
    Symbol name -> inferSymbol name
    Builtin name -> inferSymbol name
    FuncDef args expr -> inferFunc args expr
    List l -> inferList l
    Bindings b -> TRows <$> traverse infer b
    Appl f args -> inferAppl f args

-- mergeRows :: Monotype -> Monotype
-- mergeRows (List rows) = TRows . foldl' combine [] rows
--   where
--     combine m (TRows n) = m <> n
--     combine a b =
--       error $
--       "expected TRows in mergeRows but got: " ++ show a ++ " and " ++ show b
inferAppl :: AST -> [AST] -> InferM Monotype
inferAppl f args = do
  fType <- infer f
  case (fType, args) of
    (TRows rows, [expr]) -> local (<> Env rows) (infer expr)
    -- (TRowMerge, [rowList]) -> do
    -- rowTypes <- infer rowList
    -- return $ mergeRows rowTypes
    _ -> do
      argTypes <- traverse infer args
      foldM go fType argTypes >>= subM
  where
    go fType next = subM next >>= applType fType

applType :: Monotype -> Monotype -> InferM Monotype
applType (TFunc accept returnType) arg = do
  _ <- unify accept arg
  subM returnType
applType ast _ = error $ "expected TFunc but got: " ++ show ast

inferFunc :: NonEmpty String -> AST -> InferM Monotype
inferFunc (NE.toList -> args) expr = do
  argTypes <- replicateM (length args) freshVar
  let argEnv = Env . M.fromList $ zip args argTypes
  returnType <- local (<> argEnv) $ infer expr
  subM $ nestFuncs (NE.fromList argTypes) returnType
  -- where
  --   mapToFresh name = freshVar >>= return . (name, )

nestFuncs :: NonEmpty Monotype -> Monotype -> Monotype
nestFuncs (x :| []) returnType = TFunc x returnType
nestFuncs (x :| (y:ys)) returnType = TFunc x (nestFuncs (y :| ys) returnType)

inferList :: [AST] -> InferM Monotype
inferList [] = TList <$> freshVar
inferList (x:xs) = do
  (NE.nub -> (t:|ts)) <- traverse infer (x:|xs)
  TList <$> foldlM unify t ts

inferSymbol :: String -> InferM Monotype
inferSymbol name = do
  lookupSymbol name

-- boundMonotype <- freshNameAll symbolType
-- return boundMonotype
lookupSymbol :: String -> InferM Monotype
lookupSymbol name = do
  Env env <- ask
  case M.lookup name env of
    Just x -> return x
    Nothing -> throwError (UnknownIdentifier name)
-- freshNameAll :: Monotype -> InferM Monotype
-- freshNameAll t = do
--   freshMapping <- substituteAllWithFresh qs
--   pure (sub freshMapping t)
-- For each given name, add a substitution from that name to a fresh type
-- variable to the result.
  -- where
  --   substituteAllWithFresh :: Set String -> InferM Substitutions
  --   substituteAllWithFresh xs = do
  --     let freshSubstActions = M.fromSet (const freshVar) xs
  --     freshSubsts <- sequenceA freshSubstActions
  --     return . Substitutions $ freshSubsts
