{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE PatternSynonyms #-}

module AST where

import Data.Generics.Uniplate.Data ()

import Data.Functor.Classes
import GHC.Generics

import Data.Eq.Deriving
import Data.Functor.Foldable
import Generic.Data
import RIO
import Text.Show.Deriving

data AFunc from to =
  AFunc (from -> to)
  deriving (Functor, Data)

instance Show (AFunc x r) where
  show _ = "Func ..."

instance Show1 (AFunc x)

instance Eq (AFunc x r) where
  _ == _ = False

instance Eq1 (AFunc x)

type NoBindingsAST = Fix NoBindingsASTF

data NoBindingsASTF r
  = NList [r]
  | NStr String
  | NNumber Int
  | NBoolean Bool
  deriving (Functor, Data, Typeable, Generic, Generic1)

pattern FNNumber n = Fix (NNumber n)

deriveShow1 ''NoBindingsASTF

deriveEq1 ''NoBindingsASTF

data Prim
  = PList [Prim]
  | PStr String
  | PNumber Int
  | PBoolean Bool
  deriving (Typeable, Show, Eq)

type AST = Fix ASTF

data ASTF r
  = Appl r
         r
  | List [r]
  | Symbol String
  | FuncDef String
            r
  | Str String
  | Number Int
  | Boolean Bool
  | Binding String
            r
  deriving (Functor, Typeable, Show)

deriveShow1 ''ASTF

deriveEq1 ''ASTF

num :: Int -> AST
num = Fix . Number

sym :: String -> AST
sym = Fix . Symbol

funcDef :: String -> AST -> AST
funcDef argName expr = Fix $ FuncDef argName expr

curryApply :: AST -> [AST] -> AST
curryApply f (arg:[]) = Fix $ Appl f arg
curryApply f [] = error $ "Appl without any args??"
curryApply f (arg:args) = curryApply (Fix $ Appl f arg) args

type Bindings r = Map String r

type SimpleAST = Fix SimpleASTF

data SimpleASTF r
  = SList [r]
  | SFuncArg String
  | SStr String
  | SNumber Int
  | SBoolean Bool
  | SFuncDef String
             r
  | SBinding String
             r
  | SBuiltin String
             [r]
  deriving (Functor, Data, Typeable, Generic, Generic1, Show, Eq)

-- instance Show r => Show (SimpleASTF r) where
--   show (SAppl a b) = "(Appl (" ++ show a ++ ") (" ++ show b ++ "))"
--   show (SList a) = "(SList " ++ show a ++ ")"
--   show (SSymbol s) = "(SSymbol " ++ s ++ ")"
--   show (SStr s) = "(SStr " ++ s ++ ")"
--   show (SNumber n) = "(SNumber " ++ show n ++ ")"
--   show (SBoolean b) = "(SBoolean " ++ show b ++ ")"
--   show (SFunc _) = "(SFunc ...)"
--   show (SBinding name expr) = "(SBinding " ++ name ++ " (" ++ show expr ++ ")"
deriveEq1 ''SimpleASTF

sfuncDef :: String -> SimpleAST -> SimpleAST
sfuncDef n expr = Fix $ SFuncDef n expr

-- Broken orphan instance just to make life sane
instance Show1 SimpleASTF where
  liftShowsPrec = gliftShowsPrec
