{
  description = "MetaLambda - contextual modal type theory";

  nixConfig.extra-substituters = [ "https://cache.iog.io" ];

  inputs = {
    haskellNix.url = "github:input-output-hk/haskell.nix";
    nixpkgs.follows = "haskellNix/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils, haskellNix, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        index-state = "2022-11-02T00:00:00Z";
        overlays = [
          haskellNix.overlay
          (final: prev: {
            MetaLambda = final.haskell-nix.cabalProject' {
              inherit index-state;
              src = ./.;
              compiler-nix-name = "ghc924";
              shell.tools = {
                cabal = {
                  inherit index-state;
                  version = "3.8.1.0";
                };
                haskell-language-server = {
                  inherit index-state;
                  version = "1.8.0.0";
                };
                hpack = {
                  inherit index-state;
                  version = "0.35.0";
                };
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
