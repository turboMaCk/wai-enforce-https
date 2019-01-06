let
  config = {
    packageOverrides = pkgs: rec {
      haskellPackages =
        pkgs.haskellPackages.override {
          overrides = haskellPackagesNew: haskellPackagesOld: {
            wai-enforce-https =
              haskellPackagesNew.callPackage ./wai-enforce-https.nix {
                libiconv = pkgs.libiconv;
              };
          };
        };
    };
  };

  pkgs =
    import <nixpkgs> { inherit config; };
in
pkgs.haskellPackages.wai-enforce-https
