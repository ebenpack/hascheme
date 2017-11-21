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
      it "should return a ParseError" $ do
        (parse parseList "1 2 3 4 5" :: ParseResult LispVal) `shouldBe`
          (Right
             [((List [Number 1, Number 2, Number 3, Number 4, Number 5]), "")] :: ParseResult LispVal)
