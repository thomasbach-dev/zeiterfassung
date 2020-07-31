{ mkDerivation, base, hspec, parsec, stdenv, time }:
mkDerivation {
  pname = "zeiterfassung";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base parsec time ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [ base hspec parsec time ];
  license = "unknown";
  hydraPlatforms = stdenv.lib.platforms.none;
}
