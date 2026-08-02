{
  description = "Bundle a Haskell source file with its local library modules into one file";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    treefmt-nix = {
      url = "github:numtide/treefmt-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs =
    {
      self,
      nixpkgs,
      treefmt-nix,
    }:
    let
      forAllSystems =
        f:
        nixpkgs.lib.genAttrs nixpkgs.lib.systems.flakeExposed (
          system:
          let
            pkgs = nixpkgs.legacyPackages.${system};
            # Only Haskell-relevant files feed the build, so edits to
            # README.md, Justfile, flake.nix, etc. don't trigger rebuilds.
            src = nixpkgs.lib.fileset.toSource {
              root = ./.;
              fileset = nixpkgs.lib.fileset.unions [
                ./src
                ./app
                ./test
                ./bundler-hs.cabal
                ./cabal.project
              ];
            };
            pkg = pkgs.haskellPackages.callCabal2nix "bundler-hs" src { };
            treefmtEval = treefmt-nix.lib.evalModule pkgs ./treefmt.nix;
          in
          f { inherit pkgs pkg treefmtEval; }
        );
    in
    {
      packages = forAllSystems (
        { pkg, ... }: {
          default = pkg;
        }
      );

      devShells = forAllSystems (
        {
          pkgs,
          pkg,
          treefmtEval,
          ...
        }:
        {
          default = pkgs.haskellPackages.shellFor {
            packages = _: [ pkg ];
            nativeBuildInputs = [
              pkgs.haskellPackages.cabal-install
              pkgs.haskellPackages.haskell-language-server
              pkgs.just
              treefmtEval.config.build.wrapper
            ];
          };
        }
      );

      formatter = forAllSystems ({ treefmtEval, ... }: treefmtEval.config.build.wrapper);

      checks = forAllSystems (
        { pkg, treefmtEval, ... }: {
          package = pkg;
          formatting = treefmtEval.config.build.check self;
        }
      );
    };
}
