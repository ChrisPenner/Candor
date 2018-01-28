{-# language TemplateHaskell #-}
module Env where

import RIO
import AST
import Control.Lens

data Env = Env
  { _env :: Map String AST
  } deriving (Show, Eq)

makeLenses ''Env
