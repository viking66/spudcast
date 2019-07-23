{ mkDerivation, aeson, base, bytestring, directory, filepath, gogol
, gogol-storage, htaglib, lens, servant-multipart, servant-server
, stdenv, text, time, uuid, wai, wai-extra, warp
}:
mkDerivation {
  pname = "spudcast";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base bytestring directory filepath gogol gogol-storage
    htaglib lens servant-multipart servant-server text time uuid wai
    wai-extra warp
  ];
  executableHaskellDepends = [ base ];
  license = stdenv.lib.licenses.bsd3;
}
