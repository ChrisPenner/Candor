module Types where

import RIO

type TypeBindings = Map String Type

data Type =
  TNumber
    | TString
    | TBool
    | TBinder
    | TList [Type]
    | TAny
    | TBindings TypeBindings
    | TFunc [Type]
  deriving (Show, Eq)
