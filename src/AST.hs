module AST where

import qualified Data.Text as T
import qualified Data.List.NonEmpty as NE

data AST =
  Typed Type AST
  | Atom String
  | List (NE.NonEmpty AST)
  deriving Show

data Type =
  Type (NE.NonEmpty Type) (Maybe Type)
    | SingletonT String
    deriving Show
