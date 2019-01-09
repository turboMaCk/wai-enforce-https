<div align="center">
    <h1>Wai Enforce HTTPS</h1>
    <p>Safely enforce HTTPS in wai application</p>
</div>

[Wai](https://hackage.haskell.org/package/wai) middleware enforcing HTTPS connection on any incomming request.
In case of non-encrypted HTTP traffic is redirected using `301 Permanent Redirect`
(or optionally `302 Temporary Redirect`).

Middleware is compatible with reverse proxies (load balancers) like onese
used by Heroku, Google Cloud (Ingress), Azure by explicitely choosing function for detecting
protocol of a request.

## Comparision with Network.Wai.Middleware.ForceSSL

[Wai-Extra](https://hackage.haskell.org/package/wai-extra-3.0.24.3/docs/Network-Wai-Middleware-ForceSSL.html)
package comes with `Network.Wai.Middleware.ForceSSL` module exposing middleware intended for same purpose.
There are several practical weaknesses of that implementation this package intends to mitigate.

| Behaviour                      | ForceSSL (wai-extra) | EnforceHTTPS (wai-enforce-https) |
|--------------------------------|----------------------|----------------------------------|
| Possible to spoof headers      | yes                  | no                               |
| Redirecting methods by default | All                  | `GET`, `HEAD`                    |
| Redirect status                | 307                  | 301 (default) or 307 (optional)  |
| Forwarded spec compliant       | no                   | yes                              |
| Configurable port              | no                   | yes                              |
| Configurable host              | no                   | yes                              |
| 405 with `Allow` support       | no                   | yes                              |

Overall this package aims to be **secure by default** and **configurable** as much as possible.
