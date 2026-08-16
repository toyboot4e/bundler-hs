# bundler-hs

`bundler-hs` bundles a Haskell solution file and the local library modules it imports into a single file for competitive programming submissions. It handles qualified imports: `A.f` and `B.f` can coexist, renamed to `fA` and `fB`.

## Installation

Run the Nix flake directly:

```sh
$ nix run github:toyboot4e/bundler-hs
```

Or, clone the repository and run `cabal install`.

## Usage

Pass your solution file and your library directory. The bundle is printed to stdout:

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

The renaming can be customized with the `--rename-cmd` option.

In library modules, `import Prelude hiding (…)` lists are pruned if the conflicting items are renamed. Names hidden for other reasons stay hidden (with a warning).

### Language extension unification

The bundle emits the union of the `LANGUAGE` pragmas in effect for every file, that is, each file's own pragmas plus the `default-language` / `default-extensions` of its cabal project. Conflicting combinations can still fail to compile, which the bundler cannot prevent.

### CPP

CPP is handled separately from the pragma union. Directives are **preserved** into the bundle, in your own file and in library modules alike, with every branch of a library conditional renamed. Nothing is decided at bundle time, so the compiler that builds the bundle picks the branch just as it would have before bundling.

That keeps one bundle usable for both purposes. A local build whose `cpp-options` define `DEBUG` gets the debug branch, and a judge compiling the same file without them gets the other one:

```haskell
-- in a library module, and still in the bundle
#ifdef DEBUG
debug :: Bool
debug = True
#else
debug :: Bool
debug = False
#endif
```

A conditional left enclosing nothing is dropped, which happens when the imports or header pragmas between it were hoisted into the bundle's own import and pragma blocks.

#### When a library module is evaluated instead

Some modules cannot be preserved. A library module is run through the preprocessor whole when:

- It uses `#define`, `#undef`, or `#include`. A macro body is opaque text that the renamer cannot rewrite, so `#define INNER helper` sitting beside a `helper` that gets renamed would leave the expansion pointing at a name no longer there. Expansion has to come first.
- Its own cabal project supplies macros through `cpp-options` that your project does not. Those disappear along with the package, so the branch has to be decided while they are still known.
- A directive cuts through the middle of a declaration, where blanking it out would change the meaning.

The macros used for that evaluation are the ones GHC will have when it compiles the bundle. Because the bundle is a single file built inside **your** project, they come from your project's `cpp-options`, not the library's. Conditionals in the cabal file are resolved the way a plain `cabal build` resolves them, so `if flag(debug)` follows the flag's declared `default`, and `os`, `arch`, and `impl(ghc)` are decided against the host. A library project's own `cpp-options` only fill in macros your project says nothing about.

Use `-D NAME[=VALUE]` (repeatable) to supply a macro yourself. It takes precedence over both projects:

```sh
$ bundler-hs Main.hs --src path/to/your/library -D DEBUG
```

Note that `-D` only affects modules that are evaluated. A preserved conditional is the compiler's to decide, not the bundler's, so `-D` will not force one of its branches.

### Header preservation

The comments and pragmas above your module header are copied into the bundle verbatim, so a banner comment or an `{- ORMOLU_DISABLE -}` marker survives. Pragmas picked up from the cabal defaults and the library modules are appended below them.

### Formatting

The output is formatted with [hindent](https://github.com/mihaimaruseac/hindent) by default. Use the `--format-cmd` option to substitute another formatter.

> [ormolu](https://github.com/tweag/ormolu) does not work as expected. Because we parse the code and operate on the AST, the printed output has newlines in unusual places that ormolu does not handle well.

## Limitations

**The generated code is not guaranteed to compile or run correctly** even if your original code is correct. Make sure to test it before submitting. You may need to adjust your code so it still compiles under the merged imports and language extensions of the bundle.

Other known limitations:

- **Formatting is not preserved.**
- **Library comments are not preserved.** Their CPP directives are, but their comments are not.
- A library module that uses `#define`, `#undef`, or `#include` has all of its conditionals resolved at bundle time, not just the ones that need it. There is no `-U` to undefine a macro for that pass.
- Not supported (hard error): [`.hs-boot`](https://downloads.haskell.org/ghc/latest/docs/users_guide/separate_compilation.html#mutually-recursive-modules-and-hs-boot-files) files, the [`{-# SOURCE #-}`](https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/pragmas.html#source-pragma) pragma, and Template Haskell splices in library modules.

## Development

Inside the dev shell (`direnv allow` or `nix develop`):

```console
$ just build          # cabal build all
$ just test           # golden test suite
$ just test-compile   # golden suite + ghc -fno-code check of every bundle
$ just test-accept    # re-record goldens after an intentional change
$ just run Main.hs --src lib
```
