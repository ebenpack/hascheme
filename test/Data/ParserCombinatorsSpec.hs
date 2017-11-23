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
          (parse (failure "Error") x) ==
          (Left (ParseError "Error", x) :: ParseResult String)
    describe "item" $ do
      it "should take a single char" $
        property $ \x ->
          let result = parse item x
          in case x of
               [] ->
                 result ==
                 (Left (ParseError "'Item' run on empty input", []) :: ParseResult Char)
               (y:ys) -> result == (Right [(y, ys)] :: ParseResult Char)
    describe "(<|>)" $ do
      it
        "should accept the first argument that succeeds, and reject if neither succeeds" $
        property $ \x ->
          let result1 = parse (item <|> (failure "'Item' run on empty input")) x
              result2 = parse ((failure "'Item' run on empty input") <|> item) x
          in case x of
               [] ->
                 result1 == result2 &&
                 result1 ==
                 (Left (ParseError "'Item' run on empty input", []) :: ParseResult Char)
               (y:ys) ->
                 result1 == result2 &&
                 result1 == (Right [(y, ys)] :: ParseResult Char)
    describe "many'" $ do
      it "should accept zero" $ do
        (parse (many' (char '.')) "foobar" :: ParseResult [Char]) `shouldBe`
          (Right [([], "foobar")] :: ParseResult [Char])
      it "should accept one" $ do
        (parse (many' (char '.')) ".foobar" :: ParseResult [Char]) `shouldBe`
          (Right [(['.'], "foobar")] :: ParseResult [Char])
      it "should accept many" $ do
        (parse (many' (char '.')) "..........foobar" :: ParseResult [Char]) `shouldBe`
          (Right [(take 10 $ repeat '.', "foobar")] :: ParseResult [Char])
    describe "many1" $ do
      it "should not accept zero" $ do
        (parse (many1 (char '.')) "foobar" :: ParseResult [Char]) `shouldBe`
          (Left (ParseError "Condition not satisfied for: `f`", "foobar") :: ParseResult [Char])
      it "should accept one" $ do
        (parse (many1 (char '.')) ".foobar" :: ParseResult [Char]) `shouldBe`
          (Right [(['.'], "foobar")] :: ParseResult [Char])
      it "should accept many" $ do
        (parse (many1 (char '.')) "..........foobar" :: ParseResult [Char]) `shouldBe`
          (Right [(take 10 $ repeat '.', "foobar")] :: ParseResult [Char])
    describe "sepBy" $ do
      it "should accept none" $ do
        parse (sepBy (char '.') space) "foobar" `shouldBe`
          (Right [([], "foobar")] :: ParseResult [Char])
      it "should recognize one" $ do
        parse (sepBy (char '.') space) ".foobar" `shouldBe`
          (Right [(['.'], "foobar")] :: ParseResult [Char])
      it "should recognize many" $ do
        parse (sepBy (char '.') space) ". . . .foobar" `shouldBe`
          (Right [(take 4 $ repeat '.', "foobar")] :: ParseResult [Char])
    describe "endBy" $ do
      it "should accept none" $ do
        parse (endBy (char '.') space) "foobar" `shouldBe`
          (Right [([], "foobar")] :: ParseResult [Char])
      it "should recognize one" $ do
        parse (endBy (char '.') space) ". foobar" `shouldBe`
          (Right [(['.'], "foobar")] :: ParseResult [Char])
      it "should recognize many" $ do
        parse (endBy (char '.') space) ". . . . foobar" `shouldBe`
          (Right [(take 4 $ repeat '.', "foobar")] :: ParseResult [Char])
    describe "sat" $ do
      it "should accept when the predicate is satisfied" $ do
        parse (sat (== '.')) ".foobar" `shouldBe`
          (Right [('.', "foobar")] :: ParseResult Char)
      it "should reject when the predicate is not satisfied" $ do
        parse (sat (== '^')) ".foobar" `shouldBe`
          (Left (ParseError "Condition not satisfied for: `.`", ".foobar") :: ParseResult Char)
    -- describe "sat" $ do it "should do stuff" $ do 1 `shouldBe` 1
    -- describe "rej" $ do it "should do stuff" $ do 1 `shouldBe` 1
    -- describe "skipMany" $ do it "should do stuff" $ do 1 `shouldBe` 1
    -- describe "skipMany1" $ do it "should do stuff" $ do 1 `shouldBe` 1
    -- describe "oneOf" $ do it "should do stuff" $ do 1 `shouldBe` 1
    -- describe "noneOf" $ do it "should do stuff" $ do 1 `shouldBe` 1
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
