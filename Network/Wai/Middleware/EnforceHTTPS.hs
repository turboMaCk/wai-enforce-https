{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Network.Wai.Middleware.EnforceHTTPS
  ( EnforceHTTPSConfig(..)
  , defaultConfig
  , def
  , withConf
  , withResolver
  , xForwardedProto
  , azure
  , customProtoHeader
  , forwarded
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
  { httpsIsSecure        = Wai.isSecure
  , httpsHostname        = Nothing
  , httpsPort            = 443
  , httpsIgnoreURL       = False
  , httpsTemporary       = False
  , httpsSkipDefaultPort = True
  , httpsRedirectMethods = [ "GET", "HEAD" ]
  , httpsDisallowStatus  = HTTP.methodNotAllowed405
  }


withConf :: EnforceHTTPSConfig -> Middleware
withConf conf@EnforceHTTPSConfig { .. } app req
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
        Text.encodeUtf8 $ (mappend ":") $ Text.pack $ show httpsPort

    stripPort h =
      fst $ ByteString.break (== 58) h -- colon

    fullHost h = fromMaybe h httpsHostname <> port
    reqMethod = Wai.requestMethod req




def :: Middleware
def =
  withConf defaultConfig


withResolver :: HTTPSResolver -> Middleware
withResolver resolver =
  withConf $ defaultConfig { httpsIsSecure = resolver }


-- RESOLVERS


xForwardedProto :: HTTPSResolver
xForwardedProto req =
  maybe False (== "https") maybeHederVal
  where
    maybeHederVal =
      "x-forwarded-proto" `lookup` Wai.requestHeaders req


azure :: HTTPSResolver
azure req =
  maybe False (const True) maybeHeader
  where
    maybeHeader =
      "x-arr-ssl" `lookup` Wai.requestHeaders req


customProtoHeader :: ByteString -> HTTPSResolver
customProtoHeader header req =
  maybe False (== "https") maybeHederVal
  where
    maybeHederVal =
      CaseInsensitive.mk header `lookup` Wai.requestHeaders req


forwarded :: HTTPSResolver
forwarded req =
  maybe False check maybeHeader
  where
    check val =
      maybe False (== "https") $
      Forwarded.forwardedProto $ Forwarded.parseForwarded val

    maybeHeader =
      "forwarded" `lookup` Wai.requestHeaders req
