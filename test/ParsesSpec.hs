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
        Right
          (Fix $
           Appl (Fix . P $ Symbol "+") [Fix . P $ Number 1, Fix . P $ Number 2])
      parse "(- (+ 1 2) 3)" `shouldBe`
        Right
          (Fix $
           Appl
             (Fix . P $ Symbol "-")
             [ Fix $
               Appl
                 (Fix . P $ Symbol "+")
                 [Fix . P $ Number 1, Fix . P $ Number 2]
             , Fix . P $ Number 3
             ])
    it "parses bindings" $ do
      parse "(= name 42)" `shouldBe`
        Right (Fix . P $ Bindings [("name", Fix . P $ Number 42)])
    it "parses funcs" $ do
      parse "{[num] (* num 2) }" `shouldBe`
        Right
          (Fix $
           FuncDef
             ["num"]
             (Fix $
              Appl
                (Fix . P $ Symbol "*")
                [Fix . P $ Symbol "num", Fix . P $ Number 2]))
    it
      "parses definitions alongside expressions"
      (parse "((= times_two { [num] (* num 2) }) (times_two 5))" `shouldBe`
       Right
         (Fix $
          Appl
            (Fix . P $
             Bindings
               [ ( "times_two"
                 , Fix $
                   FuncDef
                     ["num"]
                     (Fix $
                      Appl
                        (Fix . P $ Symbol "*")
                        [Fix . P $ Symbol "num", Fix . P $ Number 2]))
               ])
            [Fix $ Appl (Fix . P $ Symbol "times_two") [Fix . P $ Number 5]]))
    it "parses bools" $ do
      parse "F" `shouldBe` Right (Fix . P $ Boolean False)
      parse "T" `shouldBe` Right (Fix . P $ Boolean True)
    it "parses strings" $ do
      parse "\"string\"" `shouldBe` Right (Fix . P $ Str "string")
