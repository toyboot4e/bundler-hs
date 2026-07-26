# Human-facing recipes. Run inside the dev shell (direnv loads it automatically).

# List available recipes
default:
    @just --list

# Golden suite plus a ghc -fno-code compile check of every bundle
test-compile:
    HSB_TEST_COMPILE=1 cabal test --test-show-details=direct

# Re-record golden files after an intentional output change
accept:
    cabal test --test-show-details=direct --test-options=--accept

# Build library, executable, and tests
build:
    cabal build all --enable-tests

check:
    cabal build --ghc-options="-fforce-recomp -fno-code"

# What CI runs: format check + sandboxed build with tests
ci:
    nix build .#checks.x86_64-linux.formatting .#checks.x86_64-linux.package

# Remove build artifacts
clean:
    cabal clean

# Format the whole tree (ormolu + nixfmt + cabal-fmt)
fmt:
    nix fmt

# GHCi with the library in scope
repl:
    cabal repl bundler-hs

# Bundle a file: just run Main.hs --src lib
run *ARGS:
    cabal run -v0 bundler-hs -- {{ARGS}}

# Run the golden test suite
test:
    cabal test --test-show-details=direct
