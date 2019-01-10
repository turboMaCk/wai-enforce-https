<div align="center">
    <h1>Wai Enforce HTTPS</h1>
    <p>Safely enforce HTTPS in wai application</p>
</div>

[Wai](https://hackage.haskell.org/package/wai) middleware enforcing HTTPS protocol on any incomming request.
In case of non-encrypted HTTP, traffic is redirected using `301 Permanent Redirect`
or optionally `307 Temporary Redirect`.

Middleware has compatibility modes for various reverse proxies (load balancers) and therefore can be used
with Heroku, Google Cloud (Ingress), Azure or any other type of PAS or Cloud provider.

## Comparision with ForceSSL

[Wai-Extra](https://hackage.haskell.org/package/wai-extra-3.0.24.3/docs/Network-Wai-Middleware-ForceSSL.html)
package comes with `Network.Wai.Middleware.ForceSSL` module exposing middleware intended for the same purpose.
There are several practical weaknesses of this implementation compare to one provided by wai-enforce-https.

| Behaviour                      | EnforceHTTPS (wai-enforce-https) | ForceSSL (wai-extra)             |
|--------------------------------|----------------------------------|----------------------------------|
| Redirecting methods by default | `GET`, `HEAD`                    | All                              |
| Redirect status                | 301 (default) or 307 (optional)  | 307                              |
| Safe against header spoofing   | :heavy_check_mark:  yes          | :negative_squared_cross_mark: no |
| Forwarded spec compliant       | 👍 yes                           | 👎 no                            |
| Configurable port              | ✅ yes                           | ❌ no                            |
| Configurable host              | :ballot-box-with-check: yes      | :negative_squared_cross_mark: no |
| 405 with `Allow` support       | :ballot-box-with-check: yes      | :negative_squared_cross_mark: no |

Overall, this package aims to be **secure by default** and **configurable** as much as possible
to fit any specific needs.

## Examples

This example is using [warp-tls](https://hackage.haskell.org/package/warp-tls)
and runs 2 servers:

- HTTP server on port 8080
- HTTPS server on port 8443

if you open http://127.0.0.1:8080 in browser server returns redirect
to https://127.0.0.1:8443

```haskell
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

handler :: Application
handler _ respond =
   respond $ responseLBS status200 [] "Hello over HTTPS"

httpsConf :: EnforceHTTPSConfig
httpsConf = EnforceHTTPS.defaultConfig { httpsPort = 8443 }

app :: Application
app = EnforceHTTPS.withConf httpsConf handler

main :: IO ()
main = do
  let tls = tlsSettings "examples/cert.pem" "examples/key.pem"
  _ <- forkIO $ run 8080 app
  runTLS tls (setPort (httpsPort httpsConf) defaultSettings) app
```

Another common example is running server behind reverse proxy.
Say for instance we want to host our app on [Heroku](https://heroku.com)
while using its https support and make sure we
redirect all HTTP traffic to HTTPS.
Heroku is forwarding traffic with additional header containing
information about protocol named `x-forwarded-proto`.

```haskell
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
```
