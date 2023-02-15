{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

-- | Prepare the AST for conversion to bytecode
--
-- Resolves symbols to expressions, epxands 'Appl' nodes according to type, etc.
module Desugared where

import qualified AST
import Control.Lens (ifor, makeLenses, (+~))
import Control.Monad.Except
import Control.Monad.Writer.Strict
import Data.Fix (Fix (..))
import qualified Data.Map as M
import qualified Data.Map as Map
import RIO
import Text.Show.Deriving

type Bindings = Map Text Desugared

newtype FuncName = FuncName Int
  deriving stock (Show, Eq, Ord)

newtype FuncDef = FuncDef (FuncName, Desugared, [Int])

data Env = Env
  { _scopedBindings :: Bindings,
    _nextFuncArg :: Int
  }

type NormalizeM a = WriterT [FuncDef] (ReaderT Env (Either Text)) a

data DesugaredF r
  = StrF Text
  | NumberF Int
  | BooleanF Bool
  | ListF [r]
  | IfF r r r
  | AddF r r
  | SubF r r
  | MulF r r
  | StringAppendF r r
  | EqF r r
  | FuncAppF FuncDef [r]
  | FuncLookupF FuncName
  | ArgLabelF Int

type Desugared = Fix DesugaredF

deriveShow1 ''DesugaredF

deriving instance Show r => Show (DesugaredF r)

deriving instance Show FuncDef

-- deriving stock (Show, Eq, Functor)

pattern Str :: Text -> Desugared
pattern Str s = Fix (StrF s)

pattern Number :: Int -> Desugared
pattern Number n = Fix (NumberF n)

pattern Boolean :: Bool -> Desugared
pattern Boolean b = Fix (BooleanF b)

pattern List :: [Desugared] -> Desugared
pattern List elems = Fix (ListF elems)

pattern If :: Desugared -> Desugared -> Desugared -> Desugared
pattern If cond thenBranch elseBranch = Fix (IfF cond thenBranch elseBranch)

pattern Add :: Desugared -> Desugared -> Desugared
pattern Add lhs rhs = Fix (AddF lhs rhs)

pattern Sub :: Desugared -> Desugared -> Desugared
pattern Sub lhs rhs = Fix (SubF lhs rhs)

pattern Mul :: Desugared -> Desugared -> Desugared
pattern Mul lhs rhs = Fix (MulF lhs rhs)

pattern StringAppend :: Desugared -> Desugared -> Desugared
pattern StringAppend lhs rhs = Fix (StringAppendF lhs rhs)

pattern Eq :: Desugared -> Desugared -> Desugared
pattern Eq lhs rhs = Fix (EqF lhs rhs)

pattern FuncLookup :: FuncName -> Desugared
pattern FuncLookup name = Fix (FuncLookupF name)

pattern ArgLabel :: Int -> Desugared
pattern ArgLabel n = Fix (ArgLabelF n)

makeLenses ''Env

eval :: AST.AST -> Either Text (Desugared, [FuncDef])
eval ast = runReaderT (runWriterT $ desugar' ast) $ Env mempty 0

desugar' :: AST.AST -> NormalizeM Desugared
desugar' = \case
  (AST.Symbol name) -> do
    res <- asks (M.lookup name . _scopedBindings)
    case res of
      Just expr -> pure expr
      Nothing -> throwError $ "no symbol in scope for: " <> name
  (AST.Appl (AST.Bindings binds) args) -> do
    case args of
      (expr :| []) -> do
        normBinds <- traverse desugar' binds
        local (scopedBindings %~ (normBinds <>)) $ desugar' expr
      _ -> throwError "expected single arg to Binding expression"
  (AST.Appl (AST.Builtin name) args) -> do
    case (name, args) of
      ("+", (x :| [y])) -> Add <$> desugar' x <*> desugar' y
      ("-", (x :| [y])) -> Sub <$> desugar' x <*> desugar' y
      ("*", (x :| [y])) -> Mul <$> desugar' x <*> desugar' y
      ("++", (x :| [y])) -> StringAppend <$> desugar' x <*> desugar' y
      ("==", (x :| [y])) -> Eq <$> desugar' x <*> desugar' y
      ("if", (x :| [y, z])) -> If <$> desugar' x <*> desugar' y <*> desugar' z
      _ -> throwError $ "unhandled builtin call: " <> name
  (AST.Appl expr _) -> do
    throwError $ "Unexpected expr in Appl: " <> tshow expr
  -- (AST.List elems) -> List <$> traverse desugar' elems
  (AST.Number n) -> pure $ Number n
  (AST.Str s) -> pure $ Str s
  (AST.Boolean b) -> pure $ Boolean b
  (AST.FuncDef binders body) -> funcDef binders body
  AST.Bindings {} -> throwError "Bindings outside of application"
  AST.Builtin {} -> throwError "Builtin outside of application"

funcDef :: NonEmpty Text -> AST.AST -> NormalizeM Desugared
funcDef binders body = do
  currentFuncArg <- view nextFuncArg
  argBindings <- ifor binders $ \n binder -> do
    pure (binder, ArgLabel (currentFuncArg + n))
  local
    ( \env ->
        env
          & (scopedBindings %~ \binds -> Map.fromList (toList argBindings) <> binds)
          & nextFuncArg +~ length binders
    )
    $ do desugar' body
