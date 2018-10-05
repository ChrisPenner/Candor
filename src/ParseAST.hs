{-# LANGUAGE DeriveFunctor #-}

module ParseAST where

import Data.Functor.Foldable
import RIO

data ParseASTF r
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
  deriving (Functor, Typeable, Show)

type ParseAST = Fix ParseASTF
