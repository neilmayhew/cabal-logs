{ mkDerivation, aeson, base, bytestring, containers, data-default
, filepath, lib, microlens, optparse-applicative
, regex-applicative-text, terminal-size, text, unliftio, yaml
}:
mkDerivation {
  pname = "cabal-logs";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson base bytestring containers data-default filepath microlens
    optparse-applicative regex-applicative-text terminal-size text
    unliftio yaml
  ];
  description = "Utilities for examining Cabal logs";
  license = lib.licensesSpdx."Apache-2.0";
  mainProgram = "test-failures";
}
