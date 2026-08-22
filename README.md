# bundler-hs

`bundler-hs` bundles a Haskell solution file and the local library modules it imports into a single file for competitive programming submissions. It handles qualified imports: `A.f` and `B.f` can coexist, renamed to `fA` and `fB`. Names that nothing competes for keep their original spelling, and `--tree-shake` leaves out the library code you never reach.

## Installation

Run the Nix flake directly:

```sh
$ nix run github:toyboot4e/bundler-hs
```

Or, clone the repository and run `cabal install`.

## Usage

Pass your solution file and your library directory. The bundle is printed to stdout:

```sh
$ bundler-hs Main.hs --lib path/to/your/library > submission.hs
```

See `bundler-hs --help` for the full list of options.

## Features

### Import unification and renaming

Local library modules are merged into one flat namespace, and names are renamed only as far as that takes. Each file is parsed with its own imports in scope, and the bundle merges the external imports of every file.

A name keeps its original spelling when nothing in the bundle competes for it. That takes two things. No file may import its module `qualified`, because an unqualified import means the name is already written the way it is defined. And nothing else may claim the name: not another library module, not your own top level, not Prelude, not one of your import lists, and not a name the bundle writes that only an external import can be providing.

```haskell
import Deque   -- push  ->  push
```

That last one is how an open import gets a say. `import Control.Monad.State.Class` does not list what it brings in, but a bundle that writes `modify` without any local module defining it has to be getting it from there, so a library's own `modify` moves aside. And because a kept name is never one the bundle writes with an external meaning, every kept name is also hidden from the open imports the bundle carries, which settles the ones nothing happens to write:

```haskell
import Data.List hiding (partition)   -- `partition` here is the library's
```

Every other name takes its module's suffix, and the shortest suffix that keeps the bundle collision-free wins:

1. the alias of your own `qualified ... as` import,
2. the initials of the last component of the module name,
3. that component itself,
4. the whole module name, flattened.

```haskell
import qualified SuffixArray as SA   -- build  ->  buildSA
import qualified Data.Deque          -- push   ->  pushD, else pushDeque, else pushDataDeque
```

Operators cannot carry a suffix, so they always keep their name, which makes two library modules exporting the same operator an error. Binding one alias to more than one module is an error too. Both are resolvable with `--rename-cmd`, which is told the suffix the default rule settled on (empty for a name that keeps its spelling), so `echo "$name$suffix"` reproduces the default behavior.

In library modules, `import Prelude hiding (…)` lists are pruned if the conflicting items are renamed. Names hidden for other reasons stay hidden (with a warning).

### Tree shaking

Off by default. `--tree-shake-lib` keeps only the library declarations your code actually reaches, `--tree-shake-app` does the same for your own file, and `--tree-shake` turns on both:

```sh
$ bundler-hs Main.hs --lib path/to/your/library --tree-shake-lib > submission.hs
```

Reachability is decided before renaming, so what goes does not compete for spellings either: a dropped `Util.sort` leaves `sort` free for whoever survives.

The roots are every declaration of your own file for `--tree-shake-lib`. For `--tree-shake-app` they are your module's export list, or `main` alone when the file has no export list and is `Main` (a file with no module header is `Main`). Any other module without an export list exports everything it defines, so nothing of it can go.

What a declaration needs is read generously: every name written anywhere inside it counts, local binders included, so the analysis errs towards keeping code. Only declarations that never stand on their own are dropped without being named:

- an instance goes when a local class or type of its head goes, and stays when its head is entirely external (an orphan instance is always kept),
- a type signature, a fixity declaration, an `INLINE` or `SPECIALIZE` pragma goes with the binding it annotates.

A module that loses every declaration loses its banner, its language pragmas, and its external imports along with it. Comments written directly above a dropped declaration of your own file go with it.

Preserved CPP conditionals are analysed with every branch in play, so a declaration only one branch uses still survives. A conditional left enclosing nothing is dropped like any other empty one.

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

Under `--minify-lib` a conditional would otherwise split the library section into a line before it and a line after it. Top-level order carries no meaning in Haskell, so the conditionals are moved to the end of the library section instead and everything else stays on one line. Unminified output leaves every declaration where it was written.

#### When a library module is evaluated instead

Some modules cannot be preserved. A library module is run through the preprocessor whole when:

- It uses `#define`, `#undef`, or `#include`. A macro body is opaque text that the renamer cannot rewrite, so `#define INNER helper` sitting beside a `helper` that gets renamed would leave the expansion pointing at a name no longer there. Expansion has to come first.
- Its own cabal project supplies macros through `cpp-options` that your project does not. Those disappear along with the package, so the branch has to be decided while they are still known.
- A directive cuts through the middle of a declaration, where blanking it out would change the meaning.

The macros used for that evaluation are the ones GHC will have when it compiles the bundle. Because the bundle is a single file built inside **your** project, they come from your project's `cpp-options`, not the library's. A library project's own `cpp-options` only fill in macros your project says nothing about.

Conditionals in the cabal file are resolved the way a plain `cabal build` resolves them. `os`, `arch`, and `impl(ghc)` are decided against the host, and `if flag(debug)` follows the flag's value in the build plan: the flag's declared `default`, overridden by any assignment in `cabal.project`, then `cabal.project.freeze`, then `cabal.project.local`, later files winning. Both `constraints:` entries and `package NAME` / `flags:` stanzas are read, so all of these turn the flag on:

```
constraints: my-lib +debug

package my-lib
  flags: +debug
```

Environment variables play no part, because they play no part for cabal either. `DEBUG=1 cabal build` does not define `DEBUG`. Only the flag does.

Use `-D NAME[=VALUE]` (repeatable) to supply a macro yourself. It takes precedence over both projects:

```sh
$ bundler-hs Main.hs --lib path/to/your/library -D DEBUG
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

- An **open import** (`import Data.List`) is handled by hiding every unrenamed top-level name from it, which works for values, types and classes but not for data constructors: an import list cannot name one on its own. A kept constructor that an open import also exports (`Down`, say) is still ambiguous. Rename it with `--rename-cmd`, or give that import an explicit list.
- **Formatting is not preserved.**
- **Library comments are not preserved.** Their CPP directives are, but their comments are not.
- A library module that uses `#define`, `#undef`, or `#include` has all of its conditionals resolved at bundle time, not just the ones that need it. There is no `-U` to undefine a macro for that pass.
- A `cabal.project` is only looked for from your source file up to the directory holding its `.cabal`. One sitting further up, as in some multi-package repositories, is not found.
- Not supported (hard error): [`.hs-boot`](https://downloads.haskell.org/ghc/latest/docs/users_guide/separate_compilation.html#mutually-recursive-modules-and-hs-boot-files) files, the [`{-# SOURCE #-}`](https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/pragmas.html#source-pragma) pragma, and Template Haskell splices in library modules.

## Development

Inside the dev shell (`direnv allow` or `nix develop`):

```console
$ just build          # cabal build all
$ just test           # golden test suite
$ just test-compile   # golden suite + ghc -fno-code check of every bundle
$ just test-accept    # re-record goldens after an intentional change
$ just run Main.hs --lib lib
```
