# https://github.com/casey/just

# List available recipes
default:
    @just --list

alias tc := test-compile

alias b := build

# Build library, executable, and tests
build:
    cabal build all --enable-tests

alias c := check

check:
    cabal build --ghc-options="-fforce-recomp -fno-code"

# What CI runs: format check + sandboxed build with tests
ci:
    nix build .#checks.x86_64-linux.formatting .#checks.x86_64-linux.package

alias cl := clean

# Remove build artifacts
clean:
    cabal clean

alias f := fmt

# Format the whole tree (ormolu + nixfmt + cabal-fmt)
fmt:
    nix fmt

alias re := repl

# GHCi with the library in scope
repl:
    cabal repl bundler-hs

alias r := run

# Bundle a file: just run Main.hs --src lib
run *ARGS:
    cabal run -v0 bundler-hs -- {{ARGS}}

alias t := test

# Run the golden test suite
test:
    cabal test --test-show-details=direct

alias ta := test-accept

# Re-record golden files after an intentional output change
test-accept:
    cabal test --test-show-details=direct --test-options=--accept

# Golden suite plus a ghc -fno-code compile check of every bundle
test-compile:
    HSB_TEST_COMPILE=1 cabal test --test-show-details=direct
