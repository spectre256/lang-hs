{
  description = "Toy language";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

  outputs = { nixpkgs, ... }:
    let
      system = "x86_64-linux";
      pkgs = import nixpkgs { inherit system; };
      # lang-hs = pkgs.haskellPackages.developPackage {
      #   root = ./.;
      #   modifier = drv:
      #     pkgs.haskell.lib.addBuildTools drv (with pkgs.haskellPackages; [
      #       cabal-install
      #       haskell-language-server
      #       pkgs.libllvm
      #     ]);
      # };
    in {
      # packages.${system}.default = lang-hs;

      formatter.${system} = pkgs.nixpkgs-fmt;

      devShells.${system}.default = pkgs.mkShell {
        buildInputs = with pkgs; [
          haskellPackages.cabal-install
          haskellPackages.haskell-language-server
          libllvm
        ];
      };
    };

    # nixConfig.allowBroken = true;
}
