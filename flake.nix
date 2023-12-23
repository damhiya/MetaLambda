{
  description = "MetaLambda - contextual modal type theory";

  nixConfig = {
    extra-substituters = [ "https://cache.iog.io" ];
    extra-trusted-public-keys =
      [ "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=" ];
  };

  inputs = {
    haskellNix.url = "github:input-output-hk/haskell.nix/84a8f0e7b2060f647942b60f576692df0caa63b2";
    nixpkgs.follows = "haskellNix/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils, haskellNix, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        overlays = [
          haskellNix.overlay
          (final: prev: {
            MetaLambda = final.haskell-nix.cabalProject' {
              # inherit index-state;
              src = ./.;
              compiler-nix-name = "ghc963";
              shell.tools = {
                cabal = { };
                haskell-language-server = { };
              };
            };
          })
        ];
        pkgs = import nixpkgs {
          inherit system overlays;
          inherit (haskellNix) config;
        };
        flake = pkgs.MetaLambda.flake { };
      in flake // rec {
        packages.default = flake.packages."MetaLambda:exe:MetaLambda";
        apps.default = {
          type = "app";
          program = "${packages.default}/bin/MetaLambda";
        };
      });
}
