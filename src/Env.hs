{-# language TemplateHaskell #-}
module Env where

import RIO
import AST
import Data.Map
import Control.Lens

data Env = Env 
  { _env :: Map String AST
  } deriving (Show, Eq)

makeLenses ''Env
