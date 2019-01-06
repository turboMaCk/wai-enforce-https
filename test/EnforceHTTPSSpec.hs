{-# LANGUAGE OverloadedStrings #-}

module EnforceHTTPSSpec where

import           Network.HTTP.Types
import           Network.Wai
import           Network.Wai.Middleware.EnforceHTTPS
import           Network.Wai.Test
import           Test.Hspec


app :: EnforceHTTPSConfig -> Application
app conf = enforceHTTPS conf $
  \_ respond -> respond $ responseLBS status200 [] "OK"


run :: EnforceHTTPSConfig -> Session a -> IO a
run = flip runSession . app


baseReq :: Request
baseReq =
  defaultRequest { requestHeaderHost = Just "haskell.org" }


spec :: Spec
spec = do
  describe "Default settings" $ do
    let withApp = run defaultConfig

    it "retuns 301 redirect on GET" $ withApp $ do
      res <- request $ baseReq
      assertStatus 301 res
      assertHeader "Location" "https://haskell.org" res

    it "retuns 301 redirect on HEAD" $ withApp $ do
      res <- request $ baseReq { requestMethod = "HEAD" }
      assertStatus 301 res
      assertHeader "Location" "https://haskell.org" res

    it "retuns 405 disallow on POST" $ withApp $ do
      res <- request $ baseReq { requestMethod = "POST" }
      assertStatus 405 res
      assertNoHeader "Location" res
