{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module AST where

import Data.Functor.Foldable
import Data.List.NonEmpty as NE
import Data.Map as M
import qualified Data.Text as Text
import RIO
import Text.Show.Deriving

class Pretty a where
  pretty :: a -> Text

data ASTF r
  = ApplF r (NonEmpty r)
  | StrF Text
  | NumberF Int
  | BooleanF Bool
  | SymbolF Text
  | FuncDefF (NonEmpty Text) r
  | -- | ListF [r]
    BuiltinF Text
  | BindingsF (Map Text r)
  deriving stock (Show, Eq, Ord, Functor)

newtype AST = AST (ASTF AST)
  deriving (Show, Eq, Ord)

type instance Base AST = ASTF

instance Recursive AST where
  project (AST ast) = ast

instance Corecursive AST where
  embed ast = AST ast

instance Pretty AST where
  pretty = cata \case
    ApplF f args -> "(" <> f <> Text.intercalate " " (NE.toList args) <> ")"
    StrF s -> "\"" <> s <> "\""
    NumberF n -> tshow n
    BooleanF b -> if b then "T" else "F"
    SymbolF n -> n
    FuncDefF args expr -> "{" <> tshow (NE.toList args) <> " -> " <> expr <> "}"
    -- ListF args -> "[" <> Text.intercalate ", " args <> "]"
    BuiltinF name -> name
    BindingsF binds -> "{" <> Text.intercalate ", " (showBind <$> M.toList binds) <> "}"
      where
        showBind (k, v) = k <> ": " <> v

type Bindings = Map Text AST

type EvalM a = ReaderT Bindings (Either Text) a

pattern Appl :: AST -> NonEmpty AST -> AST
pattern Appl f args = AST (ApplF f args)

pattern Str :: Text -> AST
pattern Str s = AST (StrF s)

pattern Number :: Int -> AST
pattern Number n = AST (NumberF n)

pattern Boolean :: Bool -> AST
pattern Boolean b = AST (BooleanF b)

pattern Symbol :: Text -> AST
pattern Symbol n = AST (SymbolF n)

pattern FuncDef :: NonEmpty Text -> AST -> AST
pattern FuncDef args expr = AST (FuncDefF args expr)

-- pattern List :: [AST] -> AST
-- pattern List args = AST (ListF args)

pattern Builtin :: Text -> AST
pattern Builtin name = AST (BuiltinF name)

pattern Bindings :: Bindings -> AST
pattern Bindings binds = AST (BindingsF binds)

{-# COMPLETE Appl, Str, Number, Boolean, Symbol, FuncDef, Builtin, Bindings #-}

deriveShow1 ''ASTF
