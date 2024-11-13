module Main (main) where

import Test.Hspec

main :: IO ()
main = hspec $ describe "Shortener" $ do
  it "returns True for a basic test" $ do
    True `shouldBe` True