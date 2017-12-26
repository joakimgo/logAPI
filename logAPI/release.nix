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

  nginx = let
    nginxPort = "80";
    nginxConf = pkgs.writeText "nginx.conf" ''
      user nginx nginx;
      daemon off;
      error_log /dev/stdout info;
      pid /dev/null;
      events {}
      http {
        access_log /dev/stdout;
        server {
          listen ${nginxPort};
          index index.html;
          location / {
            root ${nginxWebRoot};
          }
        }
      }
    '';
    nginxWebRoot = pkgs.writeTextDir "index.html" ''
      <html><body><h1>Hello from NGINX</h1></body></html>
    '';
  in
  pkgs.dockerTools.buildImage {
    name = "nginx-container";
    contents = pkgs.nginx;

    runAsRoot = ''
      #!${pkgs.stdenv.shell}
      ${pkgs.dockerTools.shadowSetup}
      groupadd --system nginx
      useradd --system --gid nginx nginx
    '';

    config = {
      Cmd = [ "nginx" "-c" nginxConf ];
      ExposedPorts = {
        "${nginxPort}/tcp" = {};
      };
    };
  };


in
  {
    logAPI = pkgs.haskellPackages.logAPI;
    logAPI-small = pkgs.haskellPackages.logAPI-small;
    logAPI-docker-container = pkgs.docker-container;
    logAPI-docker-container-small = pkgs.docker-container-small;
    nginxDocker = nginx;
  }
