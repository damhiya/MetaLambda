{ mkShell, ghc }:
let haskellPackages = hpkgs: with hpkgs; [ megaparsec Earley ];
in mkShell { buildInputs = [ (ghc.withPackages haskellPackages) ]; }
