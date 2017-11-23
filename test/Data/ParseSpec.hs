module Data.ParseSpec
  ( main
  , spec
  ) where

import Control.Exception (evaluate)
import Test.Hspec
import Test.QuickCheck

import DataTypes (LispVal(..))
import Parse
import ParserCombinators

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Parse" $ do
    describe "parseList" $ do
      it "should return a List" $ do
        (parse parseList "1 2 3 4 5" :: ParseResult LispVal) `shouldBe`
          (Right [(List [Number 1, Number 2, Number 3, Number 4, Number 5], "")] :: ParseResult LispVal)
      it "should return a List" $ do
        (parse parseExpr "(1 2 3 4 5)" :: ParseResult LispVal) `shouldBe`
          (Right [(List [Number 1, Number 2, Number 3, Number 4, Number 5], "")] :: ParseResult LispVal)
      it "should parse nested Lists" $ do
        (parse parseExpr "(1 (2 (3 4)))" :: ParseResult LispVal) `shouldBe`
          (Right
             [(List [Number 1, List [Number 2, List [Number 3, Number 4]]], "")] :: ParseResult LispVal)
  describe "parseDottedList" $ do
    it "should return a DottedList" $ do
      (parse parseDottedList "1 2 3 4 5 . 6" :: ParseResult LispVal) `shouldBe`
        (Right
           [ ( DottedList
                 [Number 1, Number 2, Number 3, Number 4, Number 5]
                 (Number 6)
             , "")
           ] :: ParseResult LispVal)
    it "should return a DottedList" $ do
      (parse parseExpr "(1 2 3 4 5 . 6)" :: ParseResult LispVal) `shouldBe`
        (Right
           [ ( DottedList
                 [Number 1, Number 2, Number 3, Number 4, Number 5]
                 (Number 6)
             , "")
           ] :: ParseResult LispVal)
    it "should parse nested DottedLists" $ do
      (parse parseExpr "(1 . (2 . (3 . 4)))" :: ParseResult LispVal) `shouldBe`
        (Right
           [ ( DottedList
                 [Number 1]
                 (DottedList [Number 2] (DottedList [Number 3] (Number 4)))
             , "")
           ] :: ParseResult LispVal)
    it "should reject an ill-formed DottedList" $ do
      (parse parseExpr "(1 2 3 4 5 . )" :: ParseResult LispVal) `shouldBe`
        (Left (ParseError "Condition not satisfied for: ` `", " . )") :: ParseResult LispVal)
