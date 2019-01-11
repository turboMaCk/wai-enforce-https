{-# LANGUAGE OverloadedStrings #-}
module ForwardedSpec where

import           Data.ByteString        (ByteString)
import           Test.Hspec

import qualified Data.CaseInsensitive   as CI
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

      it "should encode" $ do
        F.serializeForwarded parsed `shouldBe` val

      it "parses even without spaces" $ do
        let p = F.parseForwarded "by=foo;for=bar;host=haskell.org;proto=https"
        F.forwardedBy p `valIs` "foo"
        F.forwardedFor p `valIs` "bar"
        F.forwardedHost p `valIs` "haskell.org"
        F.forwardedProto p `valIsInsensitive` "https"


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

      it "should serialize" $ do
        F.serializeForwarded parsed `shouldBe` val

  describe "properties" $ do
    it "proto part is case insensitive" $ do
      let val = "proto=HTTP"
      let parsed = F.parseForwarded val
      F.forwardedProto parsed `shouldBe` (Just "http")

    it "for part is case sensitive" $ do
      let val = "by=FOO"
      let parsed = F.parseForwarded val
      F.forwardedBy parsed `shouldNotBe` (Just "foo")
