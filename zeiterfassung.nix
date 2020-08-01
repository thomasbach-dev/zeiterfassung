{ mkDerivation, base, hspec, parsec, stdenv, text, time }:
mkDerivation {
  pname = "zeiterfassung";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base parsec text time ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [ base hspec parsec text time ];
  license = "unknown";
  hydraPlatforms = stdenv.lib.platforms.none;
}
