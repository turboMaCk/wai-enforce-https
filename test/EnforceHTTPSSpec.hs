{-# LANGUAGE OverloadedStrings #-}

module EnforceHTTPSSpec where

import           Test.Hspec


spec :: Spec
spec = do
  describe "Whatever" $ do
    it "Is ran" $ do
      (1 :: Int) `shouldBe` (1 :: Int)
