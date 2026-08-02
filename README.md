# bundler-hs

`bundler-hs` is a Haskell source file bundler for competitive programming contests. It handles qualified imports so that `A.f` and `B.f` can coexist, bundled as `fA` and `fB`.

## Usage

Basic usage:

```sh
$ bundler-hs Main.hs --src path/to/your/library > submission.hs
```

Options:

```console
bundler-hs - Haskell source bundler for competitive programming

Usage: bundler-hs FILE [--src DIR] [--rename-cmd CMD] [--format-cmd CMD | --no-format] [--minify]
                  [--minify-lib] [--minify-user-code] [--minify-import]
                  [--minify-language-extensions] [--embed-position after|before]

  Expand local library imports into one self-contained Haskell file on stdout
```

See `bundler-hs --help` for details.

**The generated code is not guaranteed to compile or run with no problem** even if your original code is correct. Make sure to test it before your submission. You may need to modify your code to be additive.

## Features

### Import unification and renaming

Each file is parsed with the union of its own imports. Imports of local library modules are renamed to avoid name conflicts.

The default suffix for a module is the alias from *your* qualified import:

```haskell
import qualified SuffixArray as SA   -- SuffixArray.build  ->  buildSA
import qualified Data.Deque          -- push  ->  pushDataDeque (no alias)
```

This behavior can be customized with the `--rename-cmd` option.

- An import alias bound to more than one module is reported an error.
- In library modules, `import Prelude hiding (…)` lists are pruned if the conflicting items are renamed. Names hidden for other reasons stay hidden (with a warning).

### Language extension unification

Each file is parsed with the union of its own `LANGUAGE` pragmas and the `default-language` / `default-extensions` of the cabal project it belongs to. The bundle emits the additive union of all of them; conflicting combinations can still fail to compile, which the bundler cannot prevent.

- In library modules, CPP's `#` directives are **evaluated at bundle time**. In the user's file, directives sitting between top-level declarations are preserved.

### Formatting

The output code is formatted with [hindent](https://github.com/mihaimaruseac/hindent) by default. This behavior can be changed with the `--format-cmd` argument.

> [ormolu](https://github.com/tweag/ormolu) would not work as expected. This is because we parse the code and operate on the AST, the pre-formatted code has newlines at unusual points.

## Limitations

- **Formatting is not preserved**.
- **Library comments are not preserved**.
- Not supported (hard error): [`.hs-boot`](https://downloads.haskell.org/ghc/latest/docs/users_guide/separate_compilation.html#mutually-recursive-modules-and-hs-boot-files) files and the [`{-# SOURCE #-}`](https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/pragmas.html#source-pragma) pragma, Template Haskell splices in library modules.

## Development

Inside the dev shell (`direnv allow` or `nix develop`):

```console
$ just build          # cabal build all
$ just test           # golden test suite
$ just test-compile   # golden suite + ghc -fno-code check of every bundle
$ just test-accept    # re-record goldens after an intentional change
$ just run Main.hs --src lib
```
