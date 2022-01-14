{
  description = "MetaLambda - contextual modal type theory";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        haskellPackages = pkgs.haskellPackages;
        packageName = "MetaLambda";
      in rec {
        packages.${packageName} =
          haskellPackages.callCabal2nix packageName self { };
        defaultPackage = self.packages.${system}.${packageName};
        devShell = pkgs.mkShell {
          buildInputs = with haskellPackages; [
            cabal-install
            haskell-language-server
          ];
          inputsFrom = builtins.attrValues self.packages.${system};
        };
      });
}
