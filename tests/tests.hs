module Main where

import Data.Char (toLower)
import Phone
import Test.Hspec
import Test.QuickCheck

newtype UpperLetter =
  UpperLetter Char
  deriving (Show)

genUpperLetter :: Gen UpperLetter
genUpperLetter = elements (map UpperLetter ['A' .. 'Z'])

instance Arbitrary UpperLetter where
  arbitrary = genUpperLetter

main :: IO ()
main =
  hspec $ do
    describe "reverseTaps" $ do
      it "a" $ do reverseTaps daPhone 'a' `shouldBe` [('2', 1)]
      it "A" $ do reverseTaps daPhone 'A' `shouldBe` [('*', 1), ('2', 1)]
      it "upper should be star plus lower" $ do
        property $ \(UpperLetter x) ->
          reverseTaps daPhone x == ('*', 1) : reverseTaps daPhone (toLower x)
