{
  description = "Bundle a Haskell source file with its local library modules into one file";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";

  outputs = { self, nixpkgs }:
    let
      forAllSystems = f:
        nixpkgs.lib.genAttrs nixpkgs.lib.systems.flakeExposed (system:
          let
            pkgs = nixpkgs.legacyPackages.${system};
            pkg = pkgs.haskellPackages.callCabal2nix "bundler-hs" ./. { };
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
