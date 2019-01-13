{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

-- |
-- Module      : Network.Wai.Middleware.EnforceHTTPS
-- Copyright   : (c) Marek Fajkus
-- License     : BSD3
--
-- Maintainer  : marek.faj@gmail.com
--
-- Wai Middleware for enforcing encrypted HTTPS connection safely.
--
-- This module is intended to be imported @qualified@
--
-- > import qualified Network.Wai.Middleware.EnforceHTTPS as EnforceHTTPS

module Network.Wai.Middleware.EnforceHTTPS
  ( def
  , withResolver
  , xForwardedProto
  , azure
  , forwarded
  , customProtoHeader
  , EnforceHTTPSConfig(..)
  , defaultConfig
  , withConfig
  ) where

import           Data.ByteString        (ByteString)
import           Data.Maybe             (fromMaybe)
import           Data.Monoid            ((<>))
import           Network.HTTP.Types     (Method, Status)
import           Network.Wai            (Application, Middleware, Request)

import qualified Data.ByteString        as ByteString
import qualified Data.CaseInsensitive   as CaseInsensitive
import qualified Data.Text              as Text
import qualified Data.Text.Encoding     as Text
import qualified Network.HTTP.Forwarded as Forwarded
import qualified Network.HTTP.Types     as HTTP
import qualified Network.Wai            as Wai


-- | === Configuration
--
-- `EnforceHTTPSConfig` does export constructor
-- which should not colide with ny other functions
-- and therefore can be exposed in import
--
-- > import Network.Wai.Middleware.EnforceHTTPS (EnforceHTTPSConfig(..))
--
-- __Default configuration is recommended__ but you're free
-- to overide any default value if you need to.
--
-- Configuration of `httpsIsSecure` can be set using `withResolver`
-- function which is prefered way for overwriting default `Resolver` .
data EnforceHTTPSConfig = EnforceHTTPSConfig
    { httpsIsSecure        :: HTTPSResolver
    , httpsHostname        :: Maybe ByteString
    , httpsPort            :: Int
    , httpsIgnoreURL       :: Bool
    , httpsTemporary       :: Bool
    , httpsSkipDefaultPort :: Bool
    , httpsRedirectMethods :: [ Method ]
    , httpsDisallowStatus  :: Status
    }


-- | Default Configuration
-- Default resolver is proxy to @Network.Wai.isSecure@ function
--
-- * uses request @Host@ header information to resolve hostname
-- * standard HTTPS port @443@
-- * redirect includes path and url params
-- * uses permanent redirect (@301@)
-- * doesn't include @port@ in @Location@ header id port is @443@
-- * redirects @GET@ and @HEAD@ methods
-- * all /other/ methods are resolved with @405@ (Method not Allowed) and with appropriete @Allowed@ header
defaultConfig :: EnforceHTTPSConfig
defaultConfig = EnforceHTTPSConfig
  { httpsIsSecure        = Wai.isSecure
  , httpsHostname        = Nothing
  , httpsPort            = 443
  , httpsIgnoreURL       = False
  , httpsTemporary       = False
  , httpsSkipDefaultPort = True
  , httpsRedirectMethods = [ "GET", "HEAD" ]
  , httpsDisallowStatus  = HTTP.methodNotAllowed405
  }


-- | Construct `Middleware` for specific `EnforceHTTPSConfig`
withConfig :: EnforceHTTPSConfig -> Middleware
withConfig conf@EnforceHTTPSConfig { .. } app req
  | httpsIsSecure req = app req
  | otherwise = redirect conf req

redirect :: EnforceHTTPSConfig -> Application
redirect EnforceHTTPSConfig { .. } req respond = respond $
  case Wai.requestHeaderHost req of
    -- A Host header field must be sent in all HTTP/1.1 request messages.
    -- A 400 (Bad Request) status code will be sent to any HTTP/1.1 request message
    -- that lacks a Host header field or contains more than one.
    -- source: https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Host
    Nothing -> Wai.responseBuilder HTTP.status400 [] mempty
    Just h  -> Wai.responseBuilder status (headers $ stripPort h) mempty

  where
    ( status, headers ) =
      if reqMethod `elem` httpsRedirectMethods then
        ( if httpsTemporary then
            HTTP.status307
          else
            HTTP.status301
        , pure . redirectURL
        )

      else
        ( httpsDisallowStatus
        , const $
          if httpsDisallowStatus == HTTP.methodNotAllowed405 then
            [ ("Allow", ByteString.intercalate ", " httpsRedirectMethods) ]
          else
            []
        )

    redirectURL h =
      ( HTTP.hLocation, "https://" <> fullHost h <> path)

    path =
      if httpsIgnoreURL then
        mempty
      else
        Wai.rawPathInfo req <> Wai.rawQueryString req

    port =
      if httpsPort == 443 && httpsSkipDefaultPort then
        ""
      else
        Text.encodeUtf8 $ (mappend ":") $ Text.pack $ show httpsPort

    stripPort h =
      fst $ ByteString.break (== 58) h -- colon

    fullHost h = fromMaybe h httpsHostname <> port
    reqMethod = Wai.requestMethod req


-- | `Middleware` with /default/ configuration.
-- See `defaultConfig` for more details.
def :: Middleware
def =
  withConfig defaultConfig


-- | Construct middleware with provided `Resolver`
-- See `Resolver` section for informations.
withResolver :: HTTPSResolver -> Middleware
withResolver resolver =
  withConfig $ defaultConfig { httpsIsSecure = resolver }


-- | === RESOLVERS
--
-- Resolvers are function used for testing
-- if Request is made over secure HTTPS protocol.
--
-- if `True` is returned from `Resolver` function
-- request is considered as being secure.
-- For `False` values redirection logic is called.
type HTTPSResolver =
  Request -> Bool


-- | Resolver checking value of @x-forwarded-proto@ HTTP header.
-- This header is for instance used by Heroku or GCP Ingress
-- among many others.
--
-- Request is secure if value of header is `https`
-- otherwise request is considered not being secure.
xForwardedProto :: HTTPSResolver
xForwardedProto req =
  maybe False (== "https") maybeHederVal
  where
    maybeHederVal =
      "x-forwarded-proto" `lookup` Wai.requestHeaders req


-- | Azure is proxying with additional
-- `x-arr-ssl` header if original protocol is HTTPS.
-- This resolver checks for the presence of this header.
azure :: HTTPSResolver
azure req =
  maybe False (const True) maybeHeader
  where
    maybeHeader =
      "x-arr-ssl" `lookup` Wai.requestHeaders req


-- | Some reverse proxies (Kong) are using
-- values similar to @x-forwarded-proto@
-- but are using different headers.
-- This resolver allows you to specify name of header
-- which should be used for the ckeck.
-- Like `xForwardedProto`, request is considered
-- as being secure if value of header is @https@.
customProtoHeader :: ByteString -> HTTPSResolver
customProtoHeader header req =
  maybe False (== "https") maybeHederVal
  where
    maybeHederVal =
      CaseInsensitive.mk header `lookup` Wai.requestHeaders req


-- | Forwarded HTTP header is relatively new standard
-- which should replaced all @x-*@ adhoc headers by
-- standardized one.
-- This resolver is using @proto=foo@ part of the header
-- and check for equality with @https@ value.
--
-- More information can be found on [MDN](https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Forwarded)
-- Complete implementation of @Forwarded@ is located in
-- @Network.HTTP.Forwarded@ module
forwarded :: HTTPSResolver
forwarded req =
  maybe False check maybeHeader
  where
    check val =
      maybe False (== "https") $
      Forwarded.forwardedProto $ Forwarded.parseForwarded val

    maybeHeader =
      "forwarded" `lookup` Wai.requestHeaders req
