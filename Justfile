# Human-facing recipes. Run inside the dev shell (direnv loads it automatically).

# List available recipes
default:
    @just --list

# Build library, executable, and tests
build:
    cabal build all --enable-tests

# Run the golden test suite
test:
    cabal test --test-show-details=direct

# Golden suite plus a ghc -fno-code compile check of every bundle
test-compile:
    HSB_TEST_COMPILE=1 cabal test --test-show-details=direct

# Re-record golden files after an intentional output change
accept:
    cabal test --test-show-details=direct --test-options=--accept

# Bundle a file: just run Main.hs --src lib
run *ARGS:
    cabal run -v0 hs-bundle -- {{ARGS}}

# GHCi with the library in scope
repl:
    cabal repl haskell-source-bundler

# Remove build artifacts
clean:
    cabal clean
