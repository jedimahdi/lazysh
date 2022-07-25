{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Main
    ( main
    ) where

import           MyLib

import           Data.Either     ( isLeft )
import           Test.Hspec
import           Test.QuickCheck

import qualified Data.Text       as T
import           Error.Diagnose  ( Diagnostic, addFile, defaultStyle, printDiagnostic )

instance Show (Diagnostic T.Text)

instance Eq (Diagnostic T.Text)

main :: IO ()
main = hspec $ do
  describe "parseAction" $ do
    it "parses single command" $ do
      parseAction "ls" `shouldBe` Right (Command "ls" [])

    it "parses single command with args" $ do
      parseAction "ls -l -a" `shouldBe` Right (Command "ls" ["-l", "-a"])

    it "fails when no input" $ do
      isLeft $ parseAction ""

    it "parses two commands piped" $ do
      parseAction "ls | grep test" `shouldBe` Right (Pipe (Command "ls" []) (Command "grep" ["test"]))

    it "parses more than two commands piped with right order" $ do
      parseAction "ls | grep test | sort | head -1"
        `shouldBe` Right (Pipe (Pipe (Pipe (Command "ls" []) (Command "grep" ["test"])) (Command "sort" [])) (Command "head" ["-1"]))

    it "fails when there is no command after pipe" $ do
      isLeft $ parseAction "ls |"

    it "fails when there is no command before pipe" $ do
      isLeft $ parseAction "| grep test"

    it "fails when two pipes in a row without commands" $ do
      isLeft $ parseAction "ls || grep test"
