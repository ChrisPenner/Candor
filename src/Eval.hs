module Eval where

import RIO
import AST
import Env
import Control.Monad.State
import Control.Lens
import Data.List.NonEmpty

type EvalM a = State Env a

setGlobals :: EvalM ()
setGlobals = do
  add "+" (Atom (Builtin add'))
  add "-" (Atom (Builtin sub'))
    where
      add' ([Number m, (Number n)]) = Number (m + n)
      add' _ = error "expected numbers in (+)"
      sub' ([Number m, Number n]) = Number (m - n)
      sub' _ = error "expected numbers in (-)"

eval :: AST -> Val
eval ast = evalState (setGlobals >> eval' ast) (Env mempty)

eval' :: AST -> EvalM Val
eval' (Typed _ _) = return Null
eval' (Atom (Symbol name)) = do
  val <- lookup' name
  case val of
    Just expr -> eval' expr
    Nothing -> error $ name ++ " is not defined"
eval' (Atom v) = return v
eval' (Appl (h :| [])) = eval' h
eval' (Appl (f :| args)) = do
  res <- eval' f
  case res of
    Builtin f' -> do
      args' <- traverse eval' args
      return $ f' args'
    Func bindings expr -> do
      let bindings' = unpackBinding <$> bindings
      _ <- zipWithM add bindings' args
      eval' expr
    _ -> error $ "expected function not expression: " ++ show res

unpackBinding :: AST -> String
unpackBinding (Atom (Symbol name)) = name
unpackBinding b = error $ "expected string in binding position, found: " ++ show b

lookup' :: String -> EvalM (Maybe AST)
lookup' key = use (env . at key)

add :: String -> AST -> EvalM ()
add key val = (env . at key) ?= val
