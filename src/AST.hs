{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}

module AST where

-- import Data.Eq.Deriving
import Data.Functor.Foldable
import RIO

type AST = Fix ASTF

data ASTF r
  = Appl r
         r
  | List [r]
  | Symbol String
  | BindingSymbol String
  | FuncDef String
            r
  | Str String
  | Number Int
  | Boolean Bool
  | Binding String
            r
  deriving (Functor, Typeable)

type Bindings r = Map String r

type SimpleAST = Fix SimpleASTF

data SimpleASTF r
  = SAppl r
          r
  | SList [r]
  | SFuncDef String
             r
  | SStr String
  | SNumber Int
  | SBoolean Bool
  | SBuiltin (SimpleAST -> r)
  | SBinding String
             r
  deriving (Functor, Typeable)

data Prim
  = PList [Prim]
  | PStr String
  | PNumber Int
  | PBoolean Bool
  deriving (Typeable, Show, Eq)
