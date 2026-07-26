# treefmt configuration, consumed by treefmt-nix from flake.nix.
{ ... }:
{
  projectRootFile = "flake.nix";

  programs.ormolu.enable = true;
  programs.nixfmt.enable = true;
  programs.cabal-fmt.enable = true;

  # Fixtures are test data: formatting them would churn golden files for no
  # benefit and formatters may reject intentionally odd inputs.
  settings.global.excludes = [ "test/fixtures/*" ];
}
