let
  config = rec {
    packageOverrides = pkgs: rec {
      spudcast-img = pkgs.dockerTools.buildImage {
        name = "spudcast";
        tag = "latest";
        fromImage = pkgs.dockerTools.pullImage{
          imageName = "alpine";
          finalImageTag = "3.10.1";
          imageDigest = "sha256:6a92cd1fcdc8d8cdec60f33dda4db2cb1fcdcacf3410a8e05b3741f44a9b5998";
          sha256 = "1whb02n37sjspxdsvxih3sl84dpnq9irfb8ifc7ls09dcxyblisz";
        };
        config.Cmd = [ "${haskellPackages.spudcast}/bin/server" ];
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
