{ mkDerivation, aeson, base, bytestring, conduit
, conduit-combinators, conduit-extra, directory, directory-tree
, filepath, lens, lucid, mtl, servant, servant-lucid
, servant-server, stdenv, text, time, transformers, wai, warp
}:
mkDerivation {
  pname = "logAPI";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson base bytestring conduit conduit-combinators conduit-extra
    directory directory-tree filepath lens lucid mtl servant
    servant-lucid servant-server text time transformers wai warp
  ];
  description = "A small web service exposing log files";
  license = stdenv.lib.licenses.bsd3;
}
