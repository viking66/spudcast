{ mkDerivation, aeson, base, directory, filepath, gogol-pubsub
, htaglib, optparse-applicative, servant-multipart, servant-server
, stdenv, text, time, uuid, wai, wai-extra, warp
}:
mkDerivation {
  pname = "spudcast";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base gogol-pubsub htaglib servant-multipart servant-server
    text time uuid wai wai-extra warp
  ];
  executableHaskellDepends = [
    base directory filepath optparse-applicative text time uuid
  ];
  license = stdenv.lib.licenses.bsd3;
}
