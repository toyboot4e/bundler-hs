# bundler-hs

`bundler-hs` is a Haskell source file bundler for competitive programming contests. It handles qualified imports so that `A.f` and `B.f` can coexist, bundled as `fA` and `fB`.

## Usage

```sh
$ bundler-hs Main.hs --src path/to/your/library > submission.hs
```

The generated code is not guaranteed to compile or run with no problem. Make sure to test it before your submission.

<details>
<summary><code>bundler-hs --help</code></summary>

```console
$ bundler-hs --help
bundler-hs - Haskell source bundler for competitive programming

Usage: bundler-hs FILE [--src DIR] [--rename-cmd CMD]
                  [--format-cmd CMD | --no-format] [--minify] [--minify-lib]
                  [--minify-user-code] [--minify-import]
                  [--minify-language-extensions] [--embed-position after|before]

  Expand local library imports into one self-contained Haskell file on stdout

Available options:
  FILE                     Haskell source file to bundle
  --src DIR                Source directory of local library modules
                           (repeatable)
  --rename-cmd CMD         External command deciding renamed names (protocol at
                           the bottom of this help)
  --format-cmd CMD         Format with a shell command (stdin to stdout) instead
                           of the builtin hindent, e.g. 'ormolu
                           --stdin-input-file Bundle.hs'
  --no-format              Emit the raw pretty-printer output without formatting
  --minify                 Minify everything except your own code: shorthand for
                           --minify-lib --minify-import
                           --minify-language-extensions
  --minify-lib             Minify the expanded library code into one layout-free
                           line (comments dropped)
  --minify-user-code       Minify your own declarations too
  --minify-import          Minify the import section into one line
  --minify-language-extensions
                           Combine all LANGUAGE pragmas into a single {-#
                           LANGUAGE A, B, ... #-} line
  --embed-position after|before
                           Where expanded library code goes relative to your own
                           (default: after)
  -h,--help                Show this help text

Minification:
  The --minify-* flags are per-section and compose with formatting:
  minified sections become layout-free lines (comments dropped),
  everything else keeps its formatted layout. The usual submission
  setup is --minify-lib: your own code stays readable while the
  expanded library shrinks. --minify is all sections at once.

--format-cmd is a plain filter: bundle on stdin, formatted bundle on stdout.

--rename-cmd is queried in lockstep via stdin/stdout, one line per name:

    kind <TAB> module <TAB> default-suffix <TAB> name  ->  new-name

  kind is value, type, con, field, op, or extmod. All fields are
  non-empty; default-suffix is what the default rule would append, so
  `echo "$name$suffix"` reproduces the default behavior.

  op:     the name is an operator; the response must be symbolic too.
  extmod: module/name are an external module; the response is the
          qualifier to import it under in the bundle.

  Example script:

    #!/bin/sh
    tab=$(printf '\t')
    while IFS="$tab" read -r kind mod suffix name; do
      case "$mod" in
        SuffixArray) printf '%sSA\n' "$name" ;;
        *)           printf '%s%s\n' "$name" "$suffix" ;;
      esac
    done
```

</details>

## Renaming

The default suffix for a module is the alias from *your* qualified import:

```haskell
import qualified SuffixArray as SA   -- SuffixArray.build  ->  buildSA
import qualified Data.Deque          -- push  ->  pushDataDeque (no alias)
```

This behavior can be customized with the `--rename-cmd` option; its query protocol and an example script are at the bottom of the `--help` output above.

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
