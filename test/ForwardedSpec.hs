{-# LANGUAGE OverloadedStrings #-}
module ForwardedSpec where

import           Data.ByteString        (ByteString)
import           Test.Hspec

import qualified Data.CaseInsensitive as CI
import qualified Network.HTTP.Forwarded as F


valIs :: Maybe ByteString -> ByteString -> Expectation
valIs a b = shouldBe a (Just b)

valIsInsensitive :: Maybe (CI.CI ByteString) -> ByteString -> Expectation
valIsInsensitive a b = shouldBe a (Just $ CI.mk b)


spec :: Spec
spec = do
  describe "parsing" $ do
    context "full header" $ do
      let val = "by=foo; for=bar; host=haskell.org; proto=https"
      let parsed = F.parseForwarded val

      it "parses" $ do
        F.forwardedBy parsed `valIs` "foo"
        F.forwardedFor parsed `valIs` "bar"
        F.forwardedHost parsed `valIs` "haskell.org"
        F.forwardedProto parsed `valIsInsensitive` "https"

      it "show in correct format" $ do
        show parsed `shouldBe` show ("Forwarded: " <> val)

      it "should encode" $ do
        F.encodeForwarded parsed `shouldBe` val

    context "only proto part" $ do
      let val = "proto=http"
      let parsed = F.parseForwarded val

      it "parses" $ do
        F.forwardedBy parsed `shouldBe` Nothing
        F.forwardedFor parsed `shouldBe` Nothing
        F.forwardedHost parsed `shouldBe` Nothing
        F.forwardedProto parsed `valIsInsensitive` "http"

      it "show in correct format" $ do
        show parsed `shouldBe` show ("Forwarded: " <> val)

      it "should encode" $ do
        F.encodeForwarded parsed `shouldBe` val

  describe "properties" $ do
    it "proto part is case insensitive" $ do
      let val = "proto=HTTP"
      let parsed = F.parseForwarded val
      F.forwardedProto parsed `shouldBe` (Just "http")

    it "for part is case sensitive" $ do
      let val = "by=FOO"
      let parsed = F.parseForwarded val
      F.forwardedBy parsed `shouldNotBe` (Just "foo")
