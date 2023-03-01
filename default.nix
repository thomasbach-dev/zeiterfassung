{ mkDerivation, aeson, base, containers, hspec, lib, parsec
, QuickCheck, quickcheck-instances, text, time
}:
mkDerivation {
  pname = "zeiterfassung";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ aeson base containers parsec text time ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [
    base hspec parsec QuickCheck quickcheck-instances text time
  ];
  license = "unknown";
  mainProgram = "zeiterfassung";
}
