module TypeCheck where

import Types
import AST
import Primitives

import RIO
import qualified Data.Map as M

runTypecheck :: AST -> Type
runTypecheck = typecheck primitiveTypes

primitiveTypes :: TypeBindings
primitiveTypes = typecheck mempty <$> primitives

typecheck :: TypeBindings -> AST -> Type
typecheck _ (Str _) = TString
typecheck _ (Number _) = TNumber
typecheck _ (Boolean _) = TBool
typecheck bindings (Symbol name) = fromMaybe (error $ "no binding found for " ++ name) (M.lookup name bindings)
typecheck _ (Binder _) = TBinder
typecheck _ (FuncDef _ _) = undefined -- infer ast
typecheck bindings (List (x:_)) = typecheck bindings x
typecheck _ (List _) = TList TAny
typecheck _ (Builtin t _) = t
typecheck bindings (Bindings m) = TBindings (typecheck bindings <$> m)
typecheck bindings (Appl f args) = 
  case typecheck bindings f of
    TBindings typeBindings -> 
      case args of
        [expr] -> typecheck (bindings <> typeBindings) expr
        _ -> error "Expected single expression in binding pattern"
    func -> applyType func (typecheck bindings <$> args)

applyType :: Type -> [Type] -> Type
applyType (TFunc (x:[])) [] = x
applyType (TFunc (x:xs)) (y:ys)
  | x == y = applyType (TFunc xs) ys
  | otherwise = error $ "typecheck failure, expected (" ++ show x ++ ") but got: (" ++ show y ++ ")"
applyType x y = error $ "can't apply non function type: " ++ show x ++ "; " ++ show y
