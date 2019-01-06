{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Network.Wai.Middleware.EnforceHTTPS
  ( EnforceHTTPSConfig
  , defaultConfig
  , enforceHTTPS
  ) where

import           Data.ByteString     (ByteString)
import           Data.Maybe          (catMaybes)
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
    { isSecure                :: HTTPSResolver
    , hostname                :: Maybe ByteString
    , port                    :: Int
    , ignoreURL               :: Bool
    , temporary               :: Bool
    , skipDefaultPort         :: Bool
    , redirectMethods         :: [ Method ]
    , internalRedirectMethods :: [ Method ]
    , disallowStatus          :: Status
    }


defaultConfig :: EnforceHTTPSConfig
defaultConfig = EnforceHTTPSConfig
  { isSecure = Wai.isSecure
  , hostname = Nothing
  , port = 443
  , ignoreURL = False
  , temporary = False
  , skipDefaultPort = True
  , redirectMethods = [ "GET", "HEAD" ]
  , internalRedirectMethods = []
  , disallowStatus = HTTP.methodNotAllowed405
  }


enforceHTTPS :: EnforceHTTPSConfig -> Middleware
enforceHTTPS conf app req =
  if Wai.isSecure req then
    app req
  else
    redirect conf req


redirect :: EnforceHTTPSConfig -> Application
redirect EnforceHTTPSConfig { .. } req respond = respond $
  case Wai.requestHeaderHost req of
    Just h  -> Wai.responseBuilder status (headers h) mempty
    -- A Host header field must be sent in all HTTP/1.1 request messages.
    -- A 400 (Bad Request) status code will be sent to any HTTP/1.1 request message
    -- that lacks a Host header field or contains more than one.
    -- source: https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Host
    Nothing -> Wai.responseBuilder HTTP.status400 [] mempty

  where
    method = Wai.requestMethod req

    (status, headers) =
      if method `elem` redirectMethods then
        ( if temporary then
            HTTP.status302
          else
            HTTP.status301
        , pure . redirectURL
        )

      else if method `elem` internalRedirectMethods then
        ( HTTP.status307, pure . redirectURL )

      else
        ( disallowStatus, const [] )

    redirectURL h =
      ( HTTP.hLocation, "https://" <> fullHost h <> path)

    path =
      if ignoreURL then
        mempty
      else
        Wai.rawPathInfo req <> Wai.rawQueryString req

    fullHost h = h <> port'

    port' =
      if port == 443 && skipDefaultPort then
        ""
      else
        TE.encodeUtf8 $ T.pack $ show port
