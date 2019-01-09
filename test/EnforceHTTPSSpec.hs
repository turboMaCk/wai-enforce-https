{-# LANGUAGE OverloadedStrings #-}

module EnforceHTTPSSpec where

import           Network.HTTP.Types
import           Network.Wai
import           Network.Wai.Middleware.EnforceHTTPS
import           Network.Wai.Test
import           Test.Hspec


app :: EnforceHTTPSConfig -> Application
app conf = withConf conf $
  -- reference to: https://en.wikipedia.org/wiki/Zork
  \_ respond -> respond $ responseLBS status200 [] "Hello, sailor"


run :: EnforceHTTPSConfig -> Session a -> IO a
run = flip runSession . app


baseReq :: Request
baseReq =
  defaultRequest { requestHeaderHost = Just "haskell.org" }


spec :: Spec
spec = do
  describe "Default settings" defaultSettingsSpec
  describe "`httpsHostname` setting" hostnameSpec
  describe "`httpsPort` setting" portSpec
  describe "`httpsIgnoreURL` setting" ignoreURLSpec
  describe "`httpsTemporary` setting" temporarySpec
  describe "`skipDefaultPort` setting" skipDefaultPortSpec
  describe "`httsRedirectMethods` setting" redirectMethodsSpec
  describe "`httpsDisallowStatus` settings" disallowStatusSpec
  describe "resolvers" resolversSpec


defaultSettingsSpec :: Spec
defaultSettingsSpec = do
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
    assertNoHeader "Location" res
    assertBody "Hello, sailor" res

  it "includes url path and params to redirect" $ withApp $ do
    res <- request $ baseReq { rawPathInfo = "/foo", rawQueryString = "?bar=baz" }
    assertStatus 301 res
    assertHeader "Location" "https://haskell.org/foo?bar=baz" res

  it "request without `Host` header fails with 400 (Bad Request)" $ withApp $ do
    res <- request defaultRequest
    assertStatus 400 res


hostnameSpec :: Spec
hostnameSpec = do
  let withApp = run $ defaultConfig { httpsHostname = Just "foo.com" }

  it "redirects to specified hostname" $ withApp $ do
    res <- request baseReq
    assertStatus 301 res
    assertHeader "Location" "https://foo.com" res

  it "redirects with path and params" $ withApp $ do
    res <- request $ baseReq { rawPathInfo = "/foo", rawQueryString = "?bar=baz" }
    assertStatus 301 res
    assertHeader "Location" "https://foo.com/foo?bar=baz" res


portSpec :: Spec
portSpec = do
  let withApp = run $ defaultConfig { httpsPort = 8443 }

  it "redirects to specified port" $ withApp $ do
    res <- request baseReq
    assertStatus 301 res
    assertHeader "Location" "https://haskell.org:8443" res

  it "redirects with path and params" $ withApp $ do
    res <- request $ baseReq { rawPathInfo = "/foo", rawQueryString = "?bar=baz" }
    assertStatus 301 res
    assertHeader "Location" "https://haskell.org:8443/foo?bar=baz" res


ignoreURLSpec :: Spec
ignoreURLSpec =
  let withApp = run $ defaultConfig { httpsIgnoreURL = True }
  in
  it "redirect without path" $ withApp $ do
    res <- request $ baseReq { rawPathInfo = "/foo", rawQueryString = "?bar=baz" }
    assertStatus 301 res
    assertHeader "Location" "https://haskell.org" res


temporarySpec :: Spec
temporarySpec =
  let withApp = run $ defaultConfig { httpsTemporary = True }
  in
  it "redirect without path" $ withApp $ do
    res <- request baseReq
    assertStatus 307 res


skipDefaultPortSpec :: Spec
skipDefaultPortSpec =
  let withApp = run $ defaultConfig { httpsSkipDefaultPort = False }
  in
  it "redirect without path" $ withApp $ do
    res <- request baseReq
    assertStatus 301 res
    assertHeader "Location" "https://haskell.org:443" res


redirectMethodsSpec :: Spec
redirectMethodsSpec = do
  let withApp = run $ defaultConfig { httpsRedirectMethods = [ "TRACE" ] }

  it "default settings for GET is overwritten" $ withApp $ do
    res <- request baseReq
    assertStatus 405 res

  it "specified methods is redirected" $ withApp $ do
    res <- request $ baseReq { requestMethod = "TRACE" }
    assertStatus 301 res


disallowStatusSpec :: Spec
disallowStatusSpec =
  let withApp = run $ defaultConfig { httpsDisallowStatus = status403 }
  in
  it "returns specified status for disallowed method" $ withApp $ do
    res <- request $ baseReq { requestMethod = "POST" }
    assertStatus 403 res


resolversSpec :: Spec
resolversSpec = do
  describe "x-forwarded-proto resolver" $ do
    let withApp = run $ defaultConfig { httpsIsSecure = xForwardedProto }

    it "retuns 301 redirect on GET without header" $ withApp $ do
      res <- request $ baseReq
      assertStatus 301 res
      assertHeader "Location" "https://haskell.org" res

    it "retuns 301 redirect on GET if header value is http" $ withApp $ do
      res <- request $ baseReq { requestHeaders = [("x-forwarded-proto", "http")]}
      assertStatus 301 res
      assertHeader "Location" "https://haskell.org" res

    it "returns 200 if header value is https" $ withApp $ do
      res <- request $ baseReq { requestHeaders = [("x-forwarded-proto", "https")]}
      assertStatus 200 res
      assertNoHeader "Location" res
      assertBody "Hello, sailor" res

  describe "azure's x-arr-ssl header" $ do
    let withApp = run $ defaultConfig { httpsIsSecure = azure }

    it "retuns 301 redirect on GET without header" $ withApp $ do
      res <- request $ baseReq
      assertStatus 301 res
      assertHeader "Location" "https://haskell.org" res

    it "returns 200 if header is present" $ withApp $ do
      res <- request $ baseReq { requestHeaders = [("x-arr-ssl", "")]}
      assertStatus 200 res
      assertNoHeader "Location" res
      assertBody "Hello, sailor" res

  describe "custom proto header resolver" $ do
    let withApp = run $ defaultConfig { httpsIsSecure = customProtoHeader("x-proto") }

    it "retuns 301 redirect on GET without header" $ withApp $ do
      res <- request $ baseReq
      assertStatus 301 res
      assertHeader "Location" "https://haskell.org" res

    it "retuns 301 redirect on GET if header value is http" $ withApp $ do
      res <- request $ baseReq { requestHeaders = [("x-proto", "http")]}
      assertStatus 301 res
      assertHeader "Location" "https://haskell.org" res

    it "returns 200 if header value is https" $ withApp $ do
      res <- request $ baseReq { requestHeaders = [("x-proto", "https")]}
      assertStatus 200 res
      assertNoHeader "Location" res
      assertBody "Hello, sailor" res

  describe "forwarded header" $ do
    let withApp = run $ defaultConfig { httpsIsSecure = forwarded }

    it "retuns 301 redirect on GET without header" $ withApp $ do
      res <- request $ baseReq
      assertStatus 301 res
      assertHeader "Location" "https://haskell.org" res

    it "retuns 301 redirect on GET if header value is http" $ withApp $ do
      res <- request $ baseReq { requestHeaders = [("forwarded", "proto=http")]}
      assertStatus 301 res
      assertHeader "Location" "https://haskell.org" res

    it "returns 200 if header value is https" $ withApp $ do
      res <- request $ baseReq { requestHeaders = [("forwarded", "proto=https")]}
      assertStatus 200 res
      assertNoHeader "Location" res
      assertBody "Hello, sailor" res
