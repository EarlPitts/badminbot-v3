{
  description = "Haskell flake";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-24.11";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        haskellPackages = pkgs.haskell.packages.ghc966;

        brainfuck-src = pkgs.fetchFromGitHub {
          owner = "EarlPitts";
          repo = "brainfuck-interpreter";
          rev = "b46db320e58e2e04dc359eaada905159d3bacee1";
          hash = "sha256-AWktL/X4T8/r1a/0PDA45gihUt3yuwJRl/xOzWMGR8s=";
        };

        myHaskellPackages = haskellPackages.override {
          overrides = self: super: {
            brainfuck-interpreter = self.callCabal2nix "brainfuck-interpreter" brainfuck-src { };
          };
        };
      in
      {
        packages.default = pkgs.haskell.lib.justStaticExecutables (
          myHaskellPackages.callCabal2nix "badminbot" ./. { }
        );

        devShells.default = pkgs.mkShell {
          packages = with haskellPackages; [
            ghc
            cabal-install
            haskell-language-server
            hlint
            cabal-fmt
            pkgs.zlib
          ];

          LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath [
            pkgs.zlib
          ];
        };
      }
    );
}
