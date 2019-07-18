{ mkDerivation, base, directory, filepath, htaglib
, optparse-applicative, stdenv, text, time, uuid
}:
mkDerivation {
  pname = "spudcast";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base htaglib text time uuid ];
  executableHaskellDepends = [
    base directory filepath optparse-applicative text time uuid
  ];
  license = stdenv.lib.licenses.bsd3;
}
