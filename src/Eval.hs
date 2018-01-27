module Eval where

import RIO
import AST
import Env
import Control.Monad.State
import Control.Lens hiding (List)
import Data.List.NonEmpty

type EvalM a = State Env a

eval :: AST -> Val
eval ast = evalState (eval' ast) (Env mempty)

eval' :: AST -> EvalM Val
eval' (Atom v) = return v
eval' (List (h :| [])) = eval' h
-- eval' (List ((Atom (Symbol name)) :| args)) = do
  -- func <- lookup' name
  -- case func of 
    -- (Just (Decl _ _ _)) -> return ()
    -- Nothing -> throwM $ "missing " ++ name
eval' e@(Decl name _ _) = add name e >> return Null
eval' _ = error "missing pattern in eval'"

lookup' :: String -> EvalM (Maybe AST)
lookup' key = use (env . at key)

add :: String -> AST -> EvalM ()
add key val = (env . at key) ?= val
