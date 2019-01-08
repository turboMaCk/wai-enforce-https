{ mkDerivation, base, bytestring, hspec, http-types, network
, stdenv, text, wai, wai-extra, libiconv, case-insensitive
}:
mkDerivation {
  pname = "wai-enforce-https";
  version = "0.0.1";
  src = ./.;
  libraryHaskellDepends = [
    base bytestring http-types network text wai case-insensitive
  ];
  executableSystemDepends = [ libiconv ];
  testHaskellDepends = [
    base hspec wai wai-extra http-types
  ];
  homepage = "https://github.com/turboMaCk/wai-enforce-https";
  description = "Enforce https requests for wai. With reverse proxy support.";
  license = stdenv.lib.licenses.bsd3;
}
