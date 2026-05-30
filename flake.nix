{
  description = "Example Haskell development environment for Zero to Nix";

  # Flake inputs
  inputs = {
    # Latest stable Nixpkgs
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
  };

  # Flake outputs
  outputs =
    { self, nixpkgs }:
    let
      # Systems supported
      allSystems = [
        "x86_64-linux" # 64-bit Intel/AMD Linux
      ];

      # Helper to provide system-specific attributes
      forAllSystems =
        f:
        nixpkgs.lib.genAttrs allSystems (
          system:
          f {
            pkgs = import nixpkgs { inherit system; };
          }
        );
    in
    {
      # Development environment output
      devShells = forAllSystems (
        { pkgs }:
        let
          hpkgs = pkgs.haskell.packages.ghc910;
        in
        {
          default = pkgs.mkShell {
            # The Nix packages provided in the environment
            packages =  [
              hpkgs.cabal-install
              hpkgs.ghc
              hpkgs.haskell-language-server
              hpkgs.cabal-fmt
              pkgs.bash
            ];
          };
        }
      );
    };
}