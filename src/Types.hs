module Types where

import RIO

data Type =
  TNumber
    | TString
    | TBool
    | TBinder
    | TList [Type]
    | TAny
    | TBindings
    | TFunc [Type]
  deriving (Show, Eq)
