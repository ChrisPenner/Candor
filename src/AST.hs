module AST where

import RIO
import qualified Data.List.NonEmpty as NE

data AST =
  Typed Type AST
  | Appl (NE.NonEmpty AST)
  | Str String
  | Number Int
  | Symbol String
  | Func [AST] AST
  | List [AST]
  | Builtin ([AST] -> AST)
  | Null

instance Show AST where
  show (Typed t ast) = "(Typed " ++ show t ++ " " ++ show ast ++ ")"
  show (Appl l) = "(Appl " ++ show l ++ ")"
  show (Str s) = "(Str " ++ s ++ ")"
  show (Number n) = "(Number " ++ show n ++ ")"
  show (Symbol sym) = "(Symbol " ++ sym ++ ")"
  show (Func args expr) = "(Func " ++ show args ++ " " ++ show expr ++ ")"
  show (List ls) = "(List " ++ show ls ++ ")"
  show (Builtin  _) = "(Builtin *)"
  show Null = "(Null)"

instance Eq AST where
  (Typed t ast) == (Typed t' ast') = (t, ast) == (t', ast')
  (Appl l) == (Appl l') = l == l'
  (Str s) == (Str s') = s == s'
  (Number n) == (Number n') = n == n'
  (Symbol sym) == (Symbol sym') = sym == sym'
  (Func args expr) == (Func args' expr') = args == args' && expr == expr'
  (List ls) == (List ls') = ls == ls'
  (Builtin  _) == (Builtin _) = True
  Null == Null = True
  _ == _ = False

data Type =
  Type (NE.NonEmpty Type) (Maybe Type)
    | SingletonT String
    deriving (Show, Eq)
