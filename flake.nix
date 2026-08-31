{
  description = "Haskell project template GHC910";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/566acc0";
  };

  outputs =
    { self, nixpkgs }:
    let
      system = "x86_64-linux";
      pkgs = import nixpkgs { inherit system; };
      hpkgs = pkgs.haskell.packages.ghc910;
    in
    {
      devShells.${system}.default = pkgs.mkShell {
        packages = [
          hpkgs.cabal-install
          hpkgs.ghc
          hpkgs.haskell-language-server
          hpkgs.cabal-fmt
        ];
      };
    };
}
