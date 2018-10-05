{-# LANGUAGE DeriveFunctor #-}

module SimpleAST where

import Data.Functor.Foldable
import RIO

data SimpleASTF r
  = List [r]
  | Str String
  | Number Int
  | Boolean Bool
  deriving (Functor, Typeable)

type SimpleAST = Fix SimpleASTF
