{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module ASTBytecode where

import AST
import Bytecode.Types
import Control.Lens hiding (List)
import Control.Monad.State
import Data.Hashable
import Data.Serialize
import RIO
import qualified RIO.ByteString as BS

data Instr sym stackOffset label funcPtr dataPtr
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
  | PushBinding sym [Instr sym stackOffset label funcPtr dataPtr]
  | PopBinding sym
  | LookupBinding sym
  | -- num args to pop
    Call Int
  | Load dataPtr
  deriving (Show, Eq)

newtype StackOffset = StackOffset Int
  deriving (Show, Eq)

newtype Label = Label Int

newtype DataPtr = DataPtr Int
  deriving stock (Show, Eq)
  deriving newtype (Num)

-- | High level instructions
type HInstr = Instr Text StackOffset Label Void DataPtr

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

interp :: AST -> InterpM [HInstr]
interp = \case
  Appl ast asts -> do
    f <- interp ast
    args <- traverse interp asts
    pure $ concat args ++ f ++ [Call (length args)]
  Str s -> storeLoad (Txt s)
  Number n -> storeLoad n
  Boolean b -> storeLoad b
  Symbol s -> pure [LookupBinding s]
  FuncDef ne ast -> _
  Builtin s -> case s of
    "+" -> pure [Add]
    "-" -> pure [Sub]
    "*" -> pure [Mul]
    "++" -> pure [StrAppend]
    "if" -> error "if"
    "==" -> pure [Eq]
    _ -> error "Unknown builtin"
  Bindings binds ->
    foldMap
      ( \(sym, ast) -> do
          instrs <- interp ast
          pure [PushBinding sym instrs, PopBinding sym]
      )
      binds
