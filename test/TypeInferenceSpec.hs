{-# language OverloadedLists #-}
module TypeInferenceSpec where

import RIO

import Test.Hspec
import AST
import TypeInference
import Primitives
import Types

spec :: Spec
spec = do
  describe "unify" $ do
    describe "unifies monotypes" $ do
      it "unifies TConsts as expected" $ do
        runInference (unify intT intT) `shouldBe` Right mempty
        runInference (unify stringT stringT) `shouldBe` Right mempty
        runInference (unify boolT boolT) `shouldBe` Right mempty
        runInference (unify bindingsT bindingsT) `shouldBe` Right mempty
        runInference (unify intT stringT) `shouldBe` Left (CannotUnify intT stringT)
      describe "TVars" $ do
        it "unifies similar type vars" $ do
          runInference (unify (TVar "a") (TVar "a")) `shouldBe` Right mempty
        it "unifies type vars to anything with a substitution" $ do
          runInference (unify (TVar "a") (TVar "b")) `shouldBe` Right ([("a", TVar "b")])
          runInference (unify (TVar "a") intT) `shouldBe` Right ([("a", intT)])
          runInference (unify (TVar "a") stringT) `shouldBe` Right ([("a", stringT)])
          runInference (unify (TVar "a") boolT) `shouldBe` Right ([("a", boolT)])
          runInference (unify intT (TVar "a")) `shouldBe` Right ([("a", intT)])
      describe "TList" $ do
        it "unifies recursively" $ do
          runInference (unify (TList intT) (TList intT)) `shouldBe` Right mempty
          runInference (unify (TList $ TVar "a") (TList intT)) `shouldBe` Right ([("a", intT)])
      describe "TFunc" $ do
        it "unifies simple functions" $ do
          runInference (unify (TFunc stringT intT) (TFunc stringT intT)) `shouldBe` Right mempty
        it "unifies functions with required substitutions" $ do
          runInference (unify (TFunc (TVar "a") (TVar "a")) (TFunc stringT intT)) `shouldBe` Left (CannotUnify stringT intT)
          runInference (unify (TFunc (TVar "a") (TFunc intT (TVar "a"))) (TFunc intT (TFunc intT intT))) `shouldBe` Right []

  describe "infer" $ do
    it "basic AST types" $ do
      runInference (infer mempty (Str "string")) `shouldBe` Right (mempty, stringT)
      runInference (infer mempty (Number 1)) `shouldBe` Right (mempty, intT)
      runInference (infer mempty (Boolean False)) `shouldBe` Right (mempty, boolT)
      runInference (infer mempty (Binder "var")) `shouldBe` Right (mempty, binderT)
      runInference (infer mempty (Bindings mempty)) `shouldBe` Right (mempty, bindingsT)
    it "Builtins" $ do
      runInference (infer (Env primitiveTypes) (Builtin "++")) `shouldBe` Right (mempty, TFunc stringT (TFunc stringT stringT))
    describe "Lists" $ do
      it "infers type of homogenous lists" $ do
        runInference (infer mempty (List [Str "a", Str "b"])) `shouldBe` Right (mempty, TList stringT)
      it "infers unused type var for empty lists" $ do
        runInference (infer mempty (List [])) `shouldBe` Right (mempty, TList (TVar "a"))
        let res = runInference $ do
              a <- infer mempty (List [])
              b <- infer mempty (List [])
              return (a, b)
        res `shouldBe` Right ((mempty, (TList (TVar "a"))), (mempty, TList (TVar "b")))
      it "errors on heterogenous lists" $ do
        runInference (infer mempty (List [Str "a", Number 1])) `shouldBe` Left (CannotUnify stringT intT)
    describe "Symbols" $ do
      it "infers type of symbols from env" $ do
        runInference (infer [("a", Forall mempty intT)] (Symbol "a")) `shouldBe` Right (mempty, intT)
    describe "FuncDefs" $ do
      it "infers type of functions" $ do
        runInference (infer mempty (FuncDef ["a"] (Symbol "a"))) `shouldBe` Right (mempty, TFunc (TVar "a") (TVar "a"))
        runInference (infer mempty (FuncDef ["a"] (Number 1))) `shouldBe` Right (mempty, TFunc (TVar "a") intT)
        runInference (infer mempty (FuncDef ["a"] (List [Number 1, Symbol "a"]))) `shouldBe` Right ([("a", intT)], TFunc intT (TList intT))
        runInference (infer mempty (FuncDef ["a", "b"] (List [Symbol "a", Symbol "b"]))) `shouldBe` Right ([("a", TVar "b")], TFunc (TVar "b") (TFunc (TVar "b") (TList (TVar "b"))))
    describe "Appl" $ do
      it "infers proper return type" $ do
        runInference (infer (Env primitiveTypes) (Appl (FuncDef ["a"] (Symbol "a")) [Number 1])) `shouldBe` Right ([("a", intT)], intT)
        runInference (infer (Env primitiveTypes)
          (Appl
            (FuncDef ["a", "b"]
              (Appl (Builtin  "+")
                [Symbol "a", Symbol "b"]))
            [Number 1, Number 2])) `shouldBe` Right (mempty, intT)
