{
  description = "MetaLambda - contextual modal type theory";

  nixConfig.extra-substituters = [ "https://cache.iog.io" ];

  inputs = {
    haskellNix.url = "github:input-output-hk/haskell.nix";
    nixpkgs.follows = "haskellNix/nixpkgs-unstable";
    flake-utils.follows = "haskellNix/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils, haskellNix, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        index-state = "2023-05-01T00:00:00Z";
        overlays = [
          haskellNix.overlay
          (final: prev: {
            MetaLambda = final.haskell-nix.cabalProject' {
              inherit index-state;
              src = ./.;
              compiler-nix-name = "ghc927";
              shell.tools = {
                cabal = {
                  inherit index-state;
                  version = "3.8.1.0";
                };
                haskell-language-server = {
                  inherit index-state;
                  version = "1.10.0.0";
                };
                hpack = {
                  inherit index-state;
                  version = "0.35.0";
                };
              };
              shell.buildInputs = with pkgs; [
                gnumake
                texlive.combined.scheme-full
              ];
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
