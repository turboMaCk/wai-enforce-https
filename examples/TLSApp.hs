{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Concurrent                  (forkIO)
import           Network.HTTP.Types                  (status200)
import           Network.Wai                         (Application, responseLBS)
import           Network.Wai.Handler.Warp            (defaultSettings, run,
                                                      setPort)
import           Network.Wai.Handler.WarpTLS         (runTLS, tlsSettings)
import           Network.Wai.Middleware.EnforceHTTPS (EnforceHTTPSConfig (..))

import qualified Network.Wai.Middleware.EnforceHTTPS as EnforceHTTPS


port :: Int
port = 8443


handler :: Application
handler _ respond =
   respond $ responseLBS status200 [] "Hello over HTTPS"


httpsConf :: EnforceHTTPSConfig
httpsConf =
  EnforceHTTPS.defaultConfig { httpsPort = port }


app :: Application
app =
  EnforceHTTPS.withConf httpsConf handler


main :: IO ()
main = do
  let tls = tlsSettings "examples/cert.pem" "examples/key.pem"
  _ <- forkIO $ run 8080 app
  runTLS tls (setPort port defaultSettings) app
