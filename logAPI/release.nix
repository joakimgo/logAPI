let
  config = rec {
    packageOverrides = pkgs: rec {
      docker-container = pkgs.dockerTools.buildImage {
        name = "logAPI-container";
        config.Cmd = [ "${haskellPackages.logAPI}/bin/logAPI" ];
      };

      docker-container-small = pkgs.dockerTools.buildImage {
        name = "logAPI-container-small";
        config.Cmd = [ "${haskellPackages.logAPI-small}/bin/logAPI" ];
      };

      haskellPackages = pkgs.haskellPackages.override {
        overrides = haskellPackagesNew: haskellPackgesOld: rec {
          logAPI = haskellPackagesNew.callPackage ./default.nix {};

          logAPI-small =
            pkgs.haskell.lib.overrideCabal
              ( pkgs.haskell.lib.justStaticExecutables
                  (haskellPackagesNew.callPackage ./default.nix {}))
              ( oldDerivation: { });
        };
      };
    };
  };

  pkgs = import <nixpkgs> { inherit config; system = "x86_64-linux"; };

in
  {
    logAPI = pkgs.haskellPackages.logAPI;
    logAPI-small = pkgs.haskellPackages.logAPI-small;
    logAPI-docker-container = pkgs.docker-container;
    logAPI-docker-container-small = pkgs.docker-container-small;
  }
