# bundler-hs

`bundler-hs` is a Haskell source file bundler for competitive programming contests. It handles qualified imports so that `A.f` and `B.f` can coexist, bundled as `fA` and `fB`.

## Usage

```sh
$ bundler-hs Main.hs --src path/to/your/library > submission.hs
```

The generated code is not guaranteed to compile or run with no problem. Make sure to test it before your submission.

## Renaming

The default suffix for a module is the alias from *your* qualified import:

```haskell
import qualified SuffixArray as SA   -- SuffixArray.build  ->  buildSA
import qualified Data.Deque          -- push  ->  pushDataDeque (no alias)
```

## Language extensions

Each file is parsed with the union of its own `LANGUAGE` pragmas and the `default-language` / `default-extensions` of the cabal project it belongs to. The bundle emits the additive union of all of them; conflicting combinations can still fail to compile, which the bundler cannot prevent.

## Rules

- In library modules, `import Prelude hiding (…)` lists are pruned if the conflicting items are renamed. Names hidden for other reasons stay hidden (with a warning).
- In library modules, CPP's `#` directives are **evaluated at bundle time**. In the user's file, directives sitting between top-level declarations are preserved.

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
