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
      it "returns a ParseError" $
        property $ \x ->
          (parse failure x) ==
          ((Left (ParseError "Error")) :: ParseResult String)
    -- describe "item" $ do it "should do stuff" $ do 1 `shouldBe` 1
    -- describe "(<|>)" $ do it "should do stuff" $ do 1 `shouldBe` 1
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
    -- describe "many'" $ do it "should do stuff" $ do 1 `shouldBe` 1
    -- describe "many1" $ do it "should do stuff" $ do 1 `shouldBe` 1
    -- describe "skipMany" $ do it "should do stuff" $ do 1 `shouldBe` 1
    -- describe "skipMany1" $ do it "should do stuff" $ do 1 `shouldBe` 1
    -- describe "oneOf" $ do it "should do stuff" $ do 1 `shouldBe` 1
    -- describe "noneOf" $ do it "should do stuff" $ do 1 `shouldBe` 1
