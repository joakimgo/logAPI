{ mkDerivation, base, conduit, conduit-combinators, directory
, lucid, servant, servant-lucid, servant-server, stdenv, text, time
, transformers, unix, wai, warp
}:
mkDerivation {
  pname = "logAPI";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base conduit conduit-combinators directory lucid servant
    servant-lucid servant-server text time transformers unix wai warp
  ];
  description = "A small web service exposing log files";
  license = stdenv.lib.licenses.bsd3;
}
