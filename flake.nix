{
  description = "A very basic flake";
  inputs = {
    haskellNix.url = "github:input-output-hk/haskell.nix/29433c04fa097b50be67c5b799eb7a87982cd900";
    flake-utils.url = "github:numtide/flake-utils/3cecb5b042f7f209c56ffd8371b2711a290ec797";
    nixpkgs.follows = "haskellNix/nixpkgs-unstable";
  };
  outputs = { self, nixpkgs, flake-utils, haskellNix }:
    flake-utils.lib.eachSystem [ "x86_64-linux" "x86_64-darwin" ] (system:
    let
      overlays = [ haskellNix.overlay
        (final: prev: {
          # This overlay adds our project to pkgs
          zeiterfassung =
            final.haskell-nix.project' {
              src = ./.;
              compiler-nix-name = "ghc8107";
              shell.tools = {
                cabal = {};
                haskell-language-server = {};
                hspec-discover = {};
              };
              
            };
        })
      ];
      pkgs = import nixpkgs { inherit system overlays; };
      flake = pkgs.zeiterfassung.flake {};
    in flake // {
      # Built by `nix build .`
      defaultPackage = flake.packages."zeiterfassung:exe:zeiterfassung";
    });
}
