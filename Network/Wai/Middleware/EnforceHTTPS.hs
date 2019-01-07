{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Network.Wai.Middleware.EnforceHTTPS
  ( EnforceHTTPSConfig(..)
  , defaultConfig
  , enforceHTTPS
  ) where

import           Data.ByteString     (ByteString)
import           Data.Maybe          (fromMaybe)
import           Data.Monoid         ((<>))
import           Network.HTTP.Types  (Method, Status)
import           Network.Wai         (Application, Middleware, Request)

import qualified Data.Text           as T
import qualified Data.Text.Encoding  as TE
import qualified Network.HTTP.Types  as HTTP
import qualified Network.Wai         as Wai

#if __GLASGOW_HASKELL__ < 710
import           Control.Applicative ((<$>))
import           Data.Monoid         (mempty)
#endif


type HTTPSResolver =
  Request -> Bool


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


defaultConfig :: EnforceHTTPSConfig
defaultConfig = EnforceHTTPSConfig
  { httpsIsSecure = Wai.isSecure
  , httpsHostname = Nothing
  , httpsPort = 443
  , httpsIgnoreURL = False
  , httpsTemporary = False
  , httpsSkipDefaultPort = True
  , httpsRedirectMethods = [ "GET", "HEAD" ]
  , httpsDisallowStatus = HTTP.methodNotAllowed405
  }


enforceHTTPS :: EnforceHTTPSConfig -> Middleware
enforceHTTPS conf@EnforceHTTPSConfig { .. } app req =
  if httpsIsSecure req then
    app req
  else
    redirect conf req


redirect :: EnforceHTTPSConfig -> Application
redirect EnforceHTTPSConfig { .. } req respond = respond $
  case Wai.requestHeaderHost req of
    -- A Host header field must be sent in all HTTP/1.1 request messages.
    -- A 400 (Bad Request) status code will be sent to any HTTP/1.1 request message
    -- that lacks a Host header field or contains more than one.
    -- source: https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Host
    Nothing -> Wai.responseBuilder HTTP.status400 [] mempty
    Just h  -> Wai.responseBuilder status (headers h) mempty

  where
    ( status, headers ) =
      if reqMethod `elem` httpsRedirectMethods then
        ( if httpsTemporary then
            HTTP.status302
          else
            HTTP.status301
        , pure . redirectURL
        )

      else
        ( httpsDisallowStatus, const [] )

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
        TE.encodeUtf8 $ (mappend ":") $ T.pack $ show httpsPort

    fullHost h = fromMaybe h httpsHostname <> port
    reqMethod = Wai.requestMethod req
