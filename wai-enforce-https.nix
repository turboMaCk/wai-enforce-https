{ mkDerivation, base, bytestring, case-insensitive, hspec
, http-types, network, stdenv, text, wai, wai-extra, warp
, warp-tls, libiconv
}:
mkDerivation {
  pname = "wai-enforce-https";
  version = "0.0.1";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base bytestring case-insensitive http-types network text wai
  ];
  executableSystemDepends = [ libiconv ];
  executableHaskellDepends = [ base wai warp warp-tls http-types ];
  testHaskellDepends = [
    base bytestring case-insensitive hspec http-types wai wai-extra
  ];
  homepage = "https://github.com/turboMaCk/wai-enforce-https";
  description = "Enforce https requests for wai. With reverse proxy support.";
  license = stdenv.lib.licenses.bsd3;
}
