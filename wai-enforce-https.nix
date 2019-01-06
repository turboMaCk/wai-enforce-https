{ mkDerivation, base, bytestring, hspec, http-types, network
, stdenv, text, wai, libiconv
}:
mkDerivation {
  pname = "wai-enforce-https";
  version = "0.0.1";
  src = ./.;
  libraryHaskellDepends = [
    base bytestring http-types network text wai
  ];
  executableSystemDepends = [ libiconv ];
  testHaskellDepends = [ base hspec ];
  homepage = "https://github.com/turboMaCk/wai-enforce-https";
  description = "Enforce https requests for wai. With reverse proxy support.";
  license = stdenv.lib.licenses.bsd3;
}
