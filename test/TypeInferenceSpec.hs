{-# language OverloadedLists #-}
module TypeInferenceSpec where

import RIO

import Test.Hspec
import AST
import TypeInference
import Primitives
import Types
import Parse

testInference :: String -> Either InferenceError (Substitutions, Monotype)
testInference str =
  runInference (infer (Env primitiveTypes) (forceParse str))

forceParse :: String -> AST
forceParse str =
  case parse str of
    Left err -> error $ "failed to parse in test: " ++ str ++ "\n" ++ err
    Right ast' -> ast'

spec :: Spec
spec = do
  describe "inferAppl" $ do
    it "returns proper application type vars" $ do
      runInference (inferAppl (Env (primitiveTypes <> [("a", Forall mempty $ TVar "a"), ("b", Forall mempty $ TVar "b")])) (Builtin "+") [Symbol "a", Symbol "b"]) `shouldBe` Right ([("a", intT), ("b", intT)], intT)

  describe "substitutions" $ do
    it "returns expected substitutions over env vars" $ do
      runInference (infer (Env (primitiveTypes <> [("a", Forall mempty $ TVar "a"), ("b", Forall mempty $ TVar "b")])) (forceParse "(+ a b)")) `shouldBe` Right ([("a", intT), ("b", intT)], intT)
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
      testInference "\"str\"" `shouldBe` Right (mempty, stringT)
      testInference "1" `shouldBe` Right (mempty, intT)
      testInference "F" `shouldBe` Right (mempty, boolT)
      runInference (infer mempty (Bindings mempty)) `shouldBe` Right (mempty, bindingsT)
      testInference "(= a 1)" `shouldBe` Right (mempty, bindingsT)
    it "Builtins" $ do
      testInference "++" `shouldBe` Right (mempty, TFunc stringT (TFunc stringT stringT))
    describe "Lists" $ do
      it "infers type of homogenous lists" $ do
        testInference "[ 1 2 ]" `shouldBe` Right (mempty, TList intT)
      it "infers unused type var for empty lists" $ do
        testInference "[]" `shouldBe` Right (mempty, TList (TVar "a"))
        testInference "({[a] a} [])" `shouldBe` Right (mempty, TList (TVar "a"))
        let res = runInference $ do
              a <- infer mempty (List [])
              b <- infer mempty (List [])
              return (a, b)
        res `shouldBe` Right ((mempty, (TList (TVar "a"))), (mempty, TList (TVar "b")))
      it "errors on heterogenous lists" $ do
        testInference "[\"a\" 1]" `shouldBe` Left (CannotUnify stringT intT)
    describe "Symbols" $ do
      it "infers type of symbols from env" $ do
        testInference "+" `shouldBe` Right (mempty, (TFunc intT (TFunc intT intT)))
    describe "FuncDefs" $ do
      it "infers type of functions" $ do
        testInference "{[a b] (+ a b)}" `shouldBe` Right ([("a", intT), ("b", intT)], (TFunc intT (TFunc intT intT)))
        testInference "{[a] a}" `shouldBe` Right (mempty, (TFunc (TVar "a") (TVar "a")))
        testInference "{[a] 1}" `shouldBe` Right (mempty, (TFunc (TVar "a") intT))
        testInference "{[a] [1 a]}" `shouldBe` Right ([("a", intT)], (TFunc intT (TList intT)))
        testInference "{[a b] [a b]}" `shouldBe` Right ([("a", TVar "b")], (TFunc (TVar "b") (TFunc (TVar "b") (TList (TVar "b")))))
    describe "Appl" $ do
      it "infers proper return type" $ do
        testInference "({[a] a} 1)" `shouldBe` Right ([("a", intT)], intT)
        testInference "({[a b] (+ a b)} 1 2)" `shouldBe` Right ([("a", intT), ("b", intT)], intT)
