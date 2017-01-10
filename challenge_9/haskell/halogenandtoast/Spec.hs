module Spec where

import Test.Hspec
import Challenge ( squares )

main :: IO ()
main = hspec $ do
  describe "Challenge.squares" $ do
    it "handles all positives" $ do
      squares [0, 1, 2] `shouldBe` [0, 1, 4]

    it "handles all negatives" $ do
      squares [-5, -4, -3, -2] `shouldBe` [4, 9, 16, 25]

    it "handles both positive and negatives" $ do
      squares [-2, -1, 0, 1, 2] `shouldBe` [0, 1, 1, 4, 4]
