module TypeCheck where

import Types
import AST

import RIO

-- typecheck :: AST -> Maybe String
-- typecheck ast = case typecheck' ast of
                  -- Right _ -> Nothing
                  -- Left err -> Just err

typecheck' :: AST -> Type
typecheck' (Str _) = TString
typecheck' (Number _) = TNumber
typecheck' (Boolean _) = TBool
typecheck' (Symbol _) = TAny --error "symbol type not defined"
typecheck' (Binder _) = TBinder
typecheck' (FuncDef _ _) = undefined -- infer ast
typecheck' (List xs) = TList (typecheck' <$> xs)
typecheck' (Builtin t _) = t
typecheck' (Bindings _) = TBindings
typecheck' (Appl f l) = applyType (typecheck' f) (typecheck' <$> l)

applyType :: Type -> [Type] -> Type
applyType (TFunc (x:[])) (y:[])
  | x == y = x

applyType (TFunc (x:xs)) (y:ys)
  | x == y = applyType (TFunc xs) ys
  | otherwise = error $ "typecheck failure, expected (" ++ show x ++ ") but got: (" ++ show y ++ ")"
applyType _ _ = error "can't apply non function type"
