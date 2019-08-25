{ mkDerivation, aeson, base, bytestring, directory, filepath, gogol
, gogol-core, gogol-firestore, gogol-storage, hedgehog, hspec
, hspec-discover, htaglib, lens, mtl, servant-multipart
, servant-server, stdenv, text, time, uuid, wai, wai-extra, warp
, xml
}:
mkDerivation {
  pname = "spudcast";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base bytestring directory filepath gogol gogol-core
    gogol-firestore gogol-storage htaglib lens mtl servant-multipart
    servant-server text time uuid wai wai-extra warp xml
  ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [ base hedgehog hspec lens text time ];
  testToolDepends = [ hspec-discover ];
  license = stdenv.lib.licenses.bsd3;
}
