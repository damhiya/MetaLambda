{ mkShell, cabal-install, ghc }:
let haskellPackages = hpkgs: with hpkgs; [ megaparsec Earley ];
in mkShell { buildInputs = [ cabal-install (ghc.withPackages haskellPackages) ]; }
