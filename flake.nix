{
  description = "Bundle a Haskell source file with its local library modules into one file";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";

  outputs = { self, nixpkgs }:
    let
      systems = [ "x86_64-linux" "aarch64-linux" "aarch64-darwin" ];
      forAllSystems = f:
        nixpkgs.lib.genAttrs systems (system:
          let
            pkgs = nixpkgs.legacyPackages.${system};
            pkg = pkgs.haskellPackages.callCabal2nix "haskell-source-bundler" ./. { };
          in
          f pkgs pkg);
    in
    {
      packages = forAllSystems (pkgs: pkg: {
        default = pkg;
      });

      devShells = forAllSystems (pkgs: pkg: {
        default = pkgs.haskellPackages.shellFor {
          packages = _: [ pkg ];
          nativeBuildInputs = [
            pkgs.haskellPackages.cabal-install
            pkgs.haskellPackages.haskell-language-server
            pkgs.just
          ];
        };
      });
    };
}
