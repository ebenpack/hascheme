module Data.ParserCombinatorsSpec
  ( main
  , spec
  ) where

import Control.Exception (evaluate)
import Test.Hspec
import Test.QuickCheck

import ParserCombinators

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "ParserCombinators" $ do
    describe "failure" $ do
      it "should return a ParseError" $
        property $ \x ->
          (parse failure x) ==
          ((Left (ParseError "Error")) :: ParseResult String)
    describe "item" $ do
      it "should take a single char" $
        property $ \x ->
          let result = parse item x
          in case x of
               [] -> result == ((Left (ParseError "Error")) :: ParseResult Char)
               (y:ys) -> result == (Right [(y, ys)] :: ParseResult Char)
    describe "(<|>)" $ do
      it "should accept the first argument that succeeds, and otherwise" $
        property $ \x ->
          let result1 = parse (item <|> failure) x
              result2 = parse (failure <|> item) x
          in case x of
               [] ->
                 result1 == result2 &&
                 result1 == ((Left (ParseError "Error")) :: ParseResult Char)
               (y:ys) ->
                 result1 == result2 &&
                 result1 == (Right [(y, ys)] :: ParseResult Char)
    -- describe "many'" $ do it "should do stuff" $ do 1 `shouldBe` 1
    -- describe "many1" $ do it "should do stuff" $ do 1 `shouldBe` 1
    -- describe "skipMany" $ do it "should do stuff" $ do 1 `shouldBe` 1
    -- describe "skipMany1" $ do it "should do stuff" $ do 1 `shouldBe` 1
    -- describe "oneOf" $ do it "should do stuff" $ do 1 `shouldBe` 1
    -- describe "noneOf" $ do it "should do stuff" $ do 1 `shouldBe` 1
    -- describe "sat" $ do it "should do stuff" $ do 1 `shouldBe` 1
    -- describe "rej" $ do it "should do stuff" $ do 1 `shouldBe` 1
    -- describe "digit" $ do it "should do stuff" $ do 1 `shouldBe` 1
    -- describe "hexDigit" $ do it "should do stuff" $ do 1 `shouldBe` 1
    -- describe "octDigit" $ do it "should do stuff" $ do 1 `shouldBe` 1
    -- describe "lower" $ do it "should do stuff" $ do 1 `shouldBe` 1
    -- describe "upper" $ do it "should do stuff" $ do 1 `shouldBe` 1
    -- describe "letter" $ do it "should do stuff" $ do 1 `shouldBe` 1
    -- describe "alphanum" $ do it "should do stuff" $ do 1 `shouldBe` 1
    -- describe "space" $ do it "should do stuff" $ do 1 `shouldBe` 1
    -- describe "char" $ do it "should do stuff" $ do 1 `shouldBe` 1
    -- describe "string" $ do it "should do stuff" $ do 1 `shouldBe` 1
