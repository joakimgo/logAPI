{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc7102" }:
let
  inherit (nixpkgs) pkgs;
  ghc = pkgs.haskellPackages.ghcWithPackages (ps: with ps; [
          mtl
          lens
          http-client
          http-client-tls
          http-conduit
          HTTP
          xml
        ]);
in
pkgs.stdenv.mkDerivation {
  name = "haskell-env-0";
  buildInputs = [ ghc ];
  shellHook = "eval $(egrep ^export ${ghc}/bin/ghc)";
}
