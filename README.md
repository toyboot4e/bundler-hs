# bundler-hs

`bundler-hs` bundles a Haskell solution file and the local library modules it imports into a single, self-contained file, ready to submit to a competitive programming judge. Its distinguishing feature is support for qualified imports: `A.f` and `B.f` can coexist in the bundle, renamed to `fA` and `fB`.

## Installation

Run the Nix flake directly:

```sh
$ nix run github:toyboot4e/bundler-hs
```

Or, clone the repository and run `cabal install`.

## Usage

Give the bundler your solution file and the directory of your library; the bundled source is printed to stdout:

```sh
$ bundler-hs Main.hs --src path/to/your/library > submission.hs
```

See `bundler-hs --help` for the full list of options.

## Features

### Import unification and renaming

Imports of local library modules are renamed to avoid name conflicts. Each file is parsed with its own imports in scope, and the bundle merges the external imports of every file.

The default suffix for a module is the alias from your own qualified import (binding one alias to more than one module is an error):

```haskell
import qualified SuffixArray as SA   -- SuffixArray.build  ->  buildSA
import qualified Data.Deque          -- push  ->  pushDataDeque (no alias)
```

The suffix can be customized with the `--rename-cmd` option.

In library modules, `import Prelude hiding (…)` lists are pruned if the conflicting items are renamed. Names hidden for other reasons stay hidden (with a warning).

### Language extension unification

The bundle carries the union of the `LANGUAGE` pragmas in effect for every file. For each file, that means its own pragmas plus the `default-language` / `default-extensions` of the cabal project it belongs to. Conflicting combinations can still fail to compile, which the bundler cannot prevent.

CPP is handled separately: in library modules, `#` directives are **evaluated at bundle time**, while in the user's file, directives between top-level declarations are preserved.

### Formatting

The output is formatted with [hindent](https://github.com/mihaimaruseac/hindent) by default; use the `--format-cmd` option to substitute another formatter.

> [ormolu](https://github.com/tweag/ormolu) does not work as expected. Because we parse the code and operate on the AST, the printed output has newlines in unusual places that ormolu does not handle well.

## Limitations

**The generated code is not guaranteed to compile or run correctly** even if your original code is correct. Make sure to test it before submitting. You may need to adjust your code so it still compiles under the merged imports and language extensions of the bundle.

Other known limitations:

- **Formatting is not preserved.**
- **Library comments are not preserved.**
- The following are not supported and cause a hard error: [`.hs-boot`](https://downloads.haskell.org/ghc/latest/docs/users_guide/separate_compilation.html#mutually-recursive-modules-and-hs-boot-files) files, the [`{-# SOURCE #-}`](https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/pragmas.html#source-pragma) pragma, and Template Haskell splices in library modules.

## Development

The following commands are available inside the dev shell (`direnv allow` or `nix develop`):

```console
$ just build          # cabal build all
$ just test           # golden test suite
$ just test-compile   # golden suite + ghc -fno-code check of every bundle
$ just test-accept    # re-record goldens after an intentional change
$ just run Main.hs --src lib
```
