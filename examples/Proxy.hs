{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Network.HTTP.Types                  (status200)
import           Network.Wai                         (Application, responseLBS)
import           Network.Wai.Handler.Warp            (runEnv)

import qualified Network.Wai.Middleware.EnforceHTTPS as EnforceHTTPS

handler :: Application
handler _ respond =
   respond $ responseLBS status200 [] "Hello from behind proxy"

app :: Application
app = EnforceHTTPS.withResolver EnforceHTTPS.xForwardedProto handler

main :: IO ()
main = runEnv 8080 app
