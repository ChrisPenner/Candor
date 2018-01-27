module AST where

import RIO
import qualified Data.List.NonEmpty as NE

data AST =
  Typed Type AST
  | Atom Val
  | List (NE.NonEmpty AST)
  | Decl String [String] AST
  deriving (Show, Eq)

data Val =
  Str String
    | Number Int
    | Symbol String
    | Null
    deriving (Show, Eq)

data Type =
  Type (NE.NonEmpty Type) (Maybe Type)
    | SingletonT String
    deriving (Show, Eq)
