# Taken from https://github.com/NixOS/templates/tree/master/haskell-hello
{
  description = "unv-zeiterfassung";
  inputs = {
    nixpkgs.url =
      "github:NixOS/nixpkgs/068d4db604958d05d0b46c47f79b507d84dbc069";
  };
  outputs = { self, nixpkgs }:
    let
      supportedSystems = [ "x86_64-linux" ];
      forAllSystems = f:
        nixpkgs.lib.genAttrs supportedSystems (system: f system);
      nixpkgsFor = forAllSystems (system:
        import nixpkgs {
          inherit system;
          overlays = [ self.overlay ];
        });
      ghcVersion = "ghc94";
    in {
      overlay = (final: prev: {
        unv-zeiterfassung =
          final.pkgs.haskell.packages."${ghcVersion}".callCabal2nix
          "unv-zeiterfassung" ./. { };
      });
      packages = forAllSystems (system: {
        unv-zeiterfassung = nixpkgsFor.${system}.unv-zeiterfassung;
      });
      defaultPackage =
        forAllSystems (system: self.packages.${system}.unv-zeiterfassung);
      checks = self.packages;
      devShell = forAllSystems (system:
        let
          haskellPackages =
            nixpkgsFor.${system}.pkgs.haskell.packages."${ghcVersion}";
        in haskellPackages.shellFor {
          packages = p:
            with self.packages.${system};
            [
              # List all end leaf packages here
              unv-zeiterfassung
            ];
          withHoogle = false;
          buildInputs = with haskellPackages; [
            cabal-fmt
            cabal-install
            ormolu
          ];
          # Change the prompt to show that you are in a devShell
          shellHook = "export PS1='\\e[1;34mdev > \\e[0m'";
        });
    };
}
