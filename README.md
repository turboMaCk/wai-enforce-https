<div align="center">
    <h1>Wai Enforce HTTPS</h1>
    <a href="https://travis-ci.org/turboMaCk/wai-enforce-https">
        <img src="https://travis-ci.org/turboMaCk/wai-enforce-https.svg?branch=master" alt="build">
    </a>
    <p>Safely enforce HTTPS in wai application</p>
</div>

[Wai](https://hackage.haskell.org/package/wai) middleware enforcing HTTPS protocol on any incoming request.
In case of non-encrypted HTTP, traffic is redirected using `301 Permanent Redirect`
or optionally `307 Temporary Redirect`.

Middleware has compatibility modes for various reverse proxies (load balancers) and therefore can be used
with Heroku, Google Cloud (Ingress), Azure or any other type of PAS or Cloud provider.

## Comparison with ForceSSL

[Wai-Extra](https://hackage.haskell.org/package/wai-extra-3.0.24.3/docs/Network-Wai-Middleware-ForceSSL.html)
package comes with `Network.Wai.Middleware.ForceSSL` module exposing middleware intended for the same purpose.
There are several practical weaknesses of this implementation compare to one provided by wai-enforce-https.

| Behavior                        | EnforceHTTPS (wai-enforce-https)    | ForceSSL (wai-extra) |
|---------------------------------|-------------------------------------|----------------------|
| Redirecting methods by default  | `GET`, `HEAD` (by default)          | All                  |
| Redirect status                 | `301` (default) or `307` (optional) | `307`                |
| Safe against header spoofing    | ✔ yes                               | ❌ no                |
| Forwarded spec compliant        | ✔ yes                               | ❌ no                |
| Configurable port               | ✔ yes                               | ❌ no                |
| Configurable host               | ✔ yes                               | ❌ no                |
| 405 with `Allow` header support | ✔ yes                               | ❌ no                |

Overall, this package aims to be **secure by default** and **configurable** as much as possible
to fit any specific needs.

## Forwarded

In addition to the main functionality (enforcement of HTTPS) this package also
comes with module for parsing and encoding [Forwarded](https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Forwarded)
HTTP header.

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
app = EnforceHTTPS.withConfig httpsConf handler

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
handler _ respond = respond $
  responseLBS status200 [] "Hello from behind proxy"

app :: Application
app = EnforceHTTPS.withResolver EnforceHTTPS.xForwardedProto handler

main :: IO ()
main = runEnv 8080 app
```

### Bulding Examples

In order to run examples project must be build with `examples` flag:

```
$ cabal build -f examples
```


## Credits

Design of this library is heavily based on my other project [koa-sslify](https://github.com/turboMaCk/koa-sslify)
and is based on feedback and work of contributors of this library.
