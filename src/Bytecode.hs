{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Bytecode where

import AST
import Control.Lens hiding (List)
import Control.Monad.State
import Data.Hashable
import Data.Serialize
import qualified Normalize as N
import RIO
import qualified RIO.ByteString as BS

data Instr stackOffset label funcPtr dataPtr
  = Push stackOffset
  | Add
  | Sub
  | Mul
  | Div
  | Neg
  | StrAppend
  | Eq
  | Return
  | JumpIfFalse label
  | Call funcPtr
  | Load dataPtr
  deriving (Show, Eq)

newtype StackOffset = StackOffset Int
  deriving (Show, Eq)

newtype Label = Label Int

newtype DataPtr = DataPtr Int
  deriving stock (Show, Eq)
  deriving newtype (Num)

-- | High level instructions
type HInstr = Instr StackOffset Label Void DataPtr

data Stuff = Stuff
  { _internMap :: IntMap DataPtr,
    _staticData :: Builder,
    _nextPtr :: DataPtr
  }

data Env = Env {}

newtype InterpM a = InterpM (State Stuff a)
  deriving newtype (Functor, Applicative, Monad, MonadState Stuff)

makeLenses ''Stuff

store :: (Serialize a, Hashable a) => a -> InterpM DataPtr
store a = do
  preuse (internMap . ix (hash a)) >>= \case
    Nothing -> do
      let bs = encode a
      dp <- nextPtr <<+= fromIntegral (BS.length bs)
      internMap . at (hash a) ?= dp
      pure dp
    Just dp -> pure dp

storeLoad :: (Serialize a, Hashable a) => a -> InterpM [HInstr]
storeLoad a = do
  dp <- store a
  pure [Load dp]

interp :: N.Norm -> InterpM [HInstr]
interp = \case
  N.Str s -> storeLoad s
  N.Number n -> storeLoad n
  N.Boolean b -> storeLoad b
  N.List nos -> _
  N.Builtin s -> _
  N.Bindings map' -> _
  N.If no no' no2 -> _
  N.Add no no' -> _
  N.Sub no no' -> _
  N.Mul no no' -> _
  N.StringAppend no no' -> _
  N.Eq no no' -> _
  N.FuncApp fn nos -> _
  N.FuncLookup fn -> _
  N.ArgLabel n -> _
