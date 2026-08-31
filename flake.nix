{
  description = "Example Haskell development environment for Zero to Nix";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
  };

  outputs =
    { self, nixpkgs }:
    let
      allSystems = [
        "x86_64-linux" # 64-bit Intel/AMD Linux
      ];

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
            packages = [
              hpkgs.cabal
              hpkgs.ghc
              hpkgs.haskell-language-server
              hpkgs.cabal-fmt
            ];
          };
        }
      );
    };
}
