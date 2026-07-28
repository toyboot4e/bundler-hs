# bundler-hs

Bundle a Haskell source file with its local library modules into one
self-contained file for competitive programming judges. External (Hackage)
imports are left alone — the judge provides those packages; only *your*
library modules are expanded, recursively, with **real renaming**: every
top-level name gets a module-derived suffix and every use site is rewritten
via scope-aware name resolution, so `A.f` and `B.f` coexist as `fA` and `fB`.

Built on [ghc-lib-parser](https://hackage.haskell.org/package/ghc-lib-parser),
so it parses whatever recent GHC parses (GHC2021/GHC2024, `LambdaCase`, …).

## Usage

```console
$ bundler-hs Main.hs --src path/to/your/library > submission.hs
```

- `--src DIR` (repeatable): roots under which local modules are looked up.
  An import `A.B.C` is expanded iff `DIR/A/B/C.hs` exists; anything else
  stays a normal import.
- Output goes to stdout; all errors go to stderr with a non-zero exit.
- `--embed-position after|before`: where expanded library code goes
  relative to your own declarations (default `after`: your code stays at
  the top of the submission).
- Output is formatted with the builtin hindent by default (it re-decides
  every line break, so the pretty-printer's layout never shows). If
  formatting ever fails, the unformatted bundle is emitted with a warning.
  - `--no-format`: emit the raw pretty-printer output.
  - `--format-cmd CMD`: use an external formatter instead (stdin to
    stdout; CMD is a full shell command). Note that ormolu/fourmolu
    *preserve* the input's line-break decisions by design, so
    `--format-cmd 'ormolu --stdin-input-file B.hs'` normalizes spacing
    but keeps the raw layout's shape.
  - Minification is per section and composes with formatting (minified
    sections are layout-free with braces/semicolons synthesized from the
    AST; comments dropped; the rest stays formatted):
    - `--minify-lib`: the expanded library code (the usual choice - your
      own code stays readable);
    - `--minify-user-code`: your own declarations;
    - `--minify-import`: the import section;
    - `--minify-language-extensions`: one combined
      `{-# LANGUAGE A, B, ... #-}` line;
    - `--minify`: all of the above.
    Preserved user-file CPP directives keep their own lines.
  - Formatted output is always re-parsed before it reaches stdout. If a
    formatter fails, the (valid, unformatted) bundle is saved to a
    temporary file whose path is printed on stderr.

## Renaming

The default suffix for a module is the alias from *your* qualified import:

```haskell
import qualified SuffixArray as SA   -- SuffixArray.build  ->  buildSA
import qualified Data.Deque          -- push  ->  pushDataDeque (no alias)
```

Modules pulled in only transitively use their dot-stripped module name.
Everything top level is renamed — functions, types, classes, constructors,
record fields, non-exported helpers. Operators keep their names; if two
bundled modules define the same operator, bundling aborts and asks you to
resolve it via `--rename-cmd`.

### Scriptable renaming: `--rename-cmd CMD`

For full control (e.g. `SuffixArray` -> `SA` suffix regardless of import
style), pass a command. It is started once; the bundler writes one
tab-separated query per line on its stdin and reads one response line per
query from its stdout, in lockstep:

```
kind \t module \t default-suffix \t name       ->   new-name
```

- `kind` is one of `value`, `type`, `con`, `field`, `op`, `extmod`.
- For `op` the response must be a symbolic operator name.
- For `extmod` the query names an external module and the response is the
  qualifier to use for it in the bundle (default: the module name itself).
- All fields are non-empty, so a plain `while IFS=$tab read` loop works.

```sh
#!/bin/sh
tab=$(printf '\t')
while IFS="$tab" read -r kind mod suffix name; do
  case "$mod" in
    SuffixArray) printf '%sSA\n' "$name" ;;
    *)           printf '%s%s\n' "$name" "$suffix" ;;
  esac
done
```

## Language extensions

Each file is parsed with the union of its own `LANGUAGE` pragmas and the
`default-language` / `default-extensions` of the cabal project it belongs to
(found by walking up from the file / source dir). The bundle emits the
additive union of all of them; conflicting combinations can still fail to
compile, which the bundler cannot prevent.

## Guarantees and limits

- The bundler re-parses its own output before printing it; a bundle that
  does not parse never reaches stdout.
- Comments and formatting are not preserved (output is pretty-printed from
  the renamed AST).
- CPP: in library modules, `#` directives are evaluated at bundle time
  (cpphs, with `__GLASGOW_HASKELL__` defined and nothing else). In the
  user's file, directives sitting between top-level declarations are
  preserved verbatim (all branches renamed); directives cutting through
  the middle of a declaration fall back to evaluation.
- Not supported (hard error): module re-exports (`module X` in export
  lists), import cycles between local modules, Template Haskell splices in
  library modules.
- Open/`hiding` imports of external modules in library code are kept
  verbatim (a note is printed to stderr); in rare cases they can make names
  ambiguous in the bundle.

## Development

Inside the dev shell (`direnv allow` or `nix develop`):

```console
$ just build          # cabal build all
$ just test           # golden test suite
$ just test-compile   # golden suite + ghc -fno-code check of every bundle
$ just test-accept    # re-record goldens after an intentional change
$ just run Main.hs --src lib
```

Tests are golden fixtures under `test/fixtures/<case>/`: an `args` file
(CLI arguments, `{DIR}` expands to the fixture directory), input sources,
and `expected.golden` (stdout) or `expected.err.golden` (error output).
