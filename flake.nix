{
  description = "Haskell project template GHC910";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/566acc0";
    tricorder.url = "github:tweag/tricorder";
  };

  outputs =
    {
      self,
      nixpkgs,
      tricorder,
    }:
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
          tricorder.packages.${system}.tricorder
        ];
      };
    };
}
