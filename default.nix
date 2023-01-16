{ mkDerivation, aeson, base, containers, hspec, hspec-discover, lib
, parsec, text, time
}:
mkDerivation {
  pname = "zeiterfassung";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ aeson base containers parsec text time ];
  executableHaskellDepends = [
    aeson base containers parsec text time
  ];
  testHaskellDepends = [
    aeson base containers hspec parsec text time
  ];
  testToolDepends = [ hspec-discover ];
  license = "unknown";
  mainProgram = "zeiterfassung";
}
