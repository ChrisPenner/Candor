{-# LANGUAGE OverloadedLists #-}

module ParsesSpec where

import RIO

import AST
import Data.Functor.Foldable
import Parse
import Test.Hspec

spec :: Spec
spec = do
  describe "Parser" $ do
    it "parses simple arithmetic" $ do
      parse "(+ 1 2)" `shouldBe`
        Right (Fix $ Appl (Fix $ Symbol "+") [Fix $ Number 1, Fix $ Number 2])
      parse "(- (+ 1 2) 3)" `shouldBe`
        Right
          (Fix $
           Appl
             (Fix $ Symbol "-")
             [ Fix $ Appl (Fix $ Symbol "+") [Fix $ Number 1, Fix $ Number 2]
             , Fix $ Number 3
             ])
    it "parses bindings" $ do
      parse "(= name 42)" `shouldBe`
        Right (Fix $ Bindings [("name", Fix $ Number 42)])
    it "parses funcs" $ do
      parse "{[num] (* num 2) }" `shouldBe`
        Right
          (Fix $
           FuncDef
             ["num"]
             (Fix $ Appl (Fix $ Symbol "*") [Fix $ Symbol "num", Fix $ Number 2]))
    it
      "parses definitions alongside expressions"
      (parse "((= times_two { [num] (* num 2) }) (times_two 5))" `shouldBe`
       Right
         (Fix $
          Appl
            (Fix $
             Bindings
               [ ( "times_two"
                 , Fix $
                   FuncDef
                     ["num"]
                     (Fix $
                      Appl
                        (Fix $ Symbol "*")
                        [Fix $ Symbol "num", Fix $ Number 2]))
               ])
            [Fix $ Appl (Fix $ Symbol "times_two") [Fix $ Number 5]]))
    it "parses bools" $ do
      parse "F" `shouldBe` Right (Fix $ Boolean False)
      parse "T" `shouldBe` Right (Fix $ Boolean True)
    it "parses strings" $ do
      parse "\"string\"" `shouldBe` Right (Fix $ Str "string")
