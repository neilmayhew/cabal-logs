{ mkDerivation, base, cabal-plan, directory, filepath, lib
, optparse-applicative, regex-applicative, terminal-size, text
}:
mkDerivation {
  pname = "cabal-logs";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base cabal-plan directory filepath optparse-applicative
    regex-applicative terminal-size text
  ];
  description = "Utilities for examining Cabal logs";
  license = lib.licensesSpdx."Apache-2.0";
  mainProgram = "test-failures";
}
