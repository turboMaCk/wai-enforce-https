{-# LANGUAGE OverloadedStrings #-}

module EnforceHTTPSSpec where

import           Network.HTTP.Types
import           Network.Wai
import           Network.Wai.Middleware.EnforceHTTPS
import           Network.Wai.Test
import           Test.Hspec


app :: EnforceHTTPSConfig -> Application
app conf = enforceHTTPS conf $
  -- reference to: https://en.wikipedia.org/wiki/Zork
  \_ respond -> respond $ responseLBS status200 [] "Hello, sailor"


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

    it "should not redirect secure request" $ withApp $ do
      res <- request $ baseReq { isSecure = True }
      assertStatus 200 res
      assertBody "Hello, sailor" res

    it "includes url path and params to redirect" $ withApp $ do
      res <- request $ baseReq { rawPathInfo = "/foo", rawQueryString = "?bar=baz" }
      assertStatus 301 res
      assertHeader "Location" "https://haskell.org/foo?bar=baz" res


  describe "`httpsHostname` setting" $ do
    let withApp = run $ defaultConfig { httpsHostname = Just "foo.com" }

    it "redirects to specified hostname" $ withApp $ do
      res <- request baseReq
      assertStatus 301 res
      assertHeader "Location" "https://foo.com" res

    it "redirects with path and params" $ withApp $ do
      res <- request $ baseReq { rawPathInfo = "/foo", rawQueryString = "?bar=baz" }
      assertStatus 301 res
      assertHeader "Location" "https://foo.com/foo?bar=baz" res

  describe "`httpsPort` setting" $ do
    let withApp = run $ defaultConfig { httpsPort = 8443 }

    it "redirects to specified port" $ withApp $ do
      res <- request baseReq
      assertStatus 301 res
      assertHeader "Location" "https://haskell.org:8443" res

    it "redirects with path and params" $ withApp $ do
      res <- request $ baseReq { rawPathInfo = "/foo", rawQueryString = "?bar=baz" }
      assertStatus 301 res
      assertHeader "Location" "https://haskell.org:8443/foo?bar=baz" res

  describe "`httpsIgnoreURL` setting" $ do
    let withApp = run $ defaultConfig { httpsIgnoreURL = True }

    it "redirect without path" $ withApp $ do
      res <- request $ baseReq { rawPathInfo = "/foo", rawQueryString = "?bar=baz" }
      assertStatus 301 res
      assertHeader "Location" "https://haskell.org" res

  describe "`httpsTemporary` setting" $ do
    let withApp = run $ defaultConfig { httpsTemporary = True }

    it "redirect without path" $ withApp $ do
      res <- request baseReq
      assertStatus 302 res

  describe "`skipDefaultPort` setting" $ do
    let withApp = run $ defaultConfig { httpsSkipDefaultPort = False }

    it "redirect without path" $ withApp $ do
      res <- request baseReq
      assertStatus 301 res
      assertHeader "Location" "https://haskell.org:443" res

  describe "`httsRedirectMethods` setting" $ do
    let withApp = run $ defaultConfig { httpsRedirectMethods = [ "TRACE" ] }

    it "default settings for GET is overwritten" $ withApp $ do
      res <- request baseReq
      assertStatus 405 res

    it "specified methods is redirected" $ withApp $ do
      res <- request $ baseReq { requestMethod = "TRACE" }
      assertStatus 301 res

  describe "`httpsDisallowStatus` settings" $ do
    let withApp = run $ defaultConfig { httpsDisallowStatus = status403 }

    it "returns specified status for disallowed method" $ withApp $ do
      res <- request $ baseReq { requestMethod = "POST" }
      assertStatus 403 res
