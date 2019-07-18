let
  config = rec {
    packageOverrides = pkgs: rec {
      spudcast-img = pkgs.dockerTools.buildImage {
        name = "spudcast";
        tag = "latest";
        config.Cmd = [ "${haskellPackages.spudcast-run}/bin/spudcast" ];
      };

      haskellPackages = pkgs.haskellPackages.override {
        overrides = haskellPackagesNew: haskellPackgesOld: rec {
          spudcast =
            pkgs.haskell.lib.overrideCabal
              ( pkgs.haskell.lib.justStaticExecutables
                  ( haskellPackagesNew.callPackage ./spudcast.nix {
                    }
                  )
              )
              ( oldDerivation: {
                  testToolDepends = [ pkgs.libarchive ];
                }
              );
        };
      };
    };
  };

  pkgs = import <nixpkgs> { inherit config; system = "x86_64-linux"; };

in
  { spudcast = pkgs.haskellPackages.spudcast;
    spudcast-img = pkgs.spudcast-img;
  }

