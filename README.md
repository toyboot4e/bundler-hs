# bundler-hs

`bundler-hs` bundles a Haskell solution file and local library modules into a single file for competitive programming submissions.

- It handles qualified imports. `A.f` and `B.f` can coexist, renamed to `fA` and `fB`.
- Use the `--tree-shake` option to drop unused code.
- Use the `--minify` option to minify your templates in your submission.

## Installation

Run the Nix flake directly:

```sh
nix run github:toyboot4e/bundler-hs
```

Or, clone the repository and run `cabal install`.

## Usage

Pass your solution file and your library directory. The bundle is printed to stdout:

```sh
bundler-hs Main.hs --lib path/to/your/library > submission.hs
```

See `bundler-hs --help` for the full list of options.

## Features

### Import bundling

In competitive programming, you submit your `Main.hs` file only. If you want to have a separate library, you need to bundle (expand) the library modules.

For instance, say this is one of your library modules:

```haskell
-- MyLibrary/Module.hs
primeNumbers :: [Int]
primeNumbers = [2, 3, 5, 7, 11, 13]
```

And this is your solution file:

```haskell
-- Main.hs
import MyLibrary.Module (primeNumbers)

main :: IO ()
main = do
  print . take 3 $ primeNumbers
```

`bundler-hs` will bundle your solution file and your library as follows:

```haskell
main :: IO ()
main = do
  print . take 3 $ primeNumbers

-- ### MyLibrary.Module
primeNumbers :: [Int]
primeNumbers = [2, 3, 5, 7, 11, 13]
```

Notice that the `M.primeNumbers` is now expanded as `primeNumbers`.

### Renaming qualified import names

Your function name may conflict with each other. Take the following case as an example:

```haskell
-- MyLibrary/Math/MyFunc1.hs
f :: Int
f = 10
```

```haskell
-- MyLibrary/Math/MyFunc2.hs
f :: Int
f = 77
```

```haskell
-- Main.hs
import MyLibrary.Math.MyFunc1 qualified as F1
import MyLibrary.Math.MyFunc2 qualified as F2

main :: IO ()
main = print $ F1.f + F2.f
```

In such a case, each name-conflicting function will be renamed as follows:

```haskell
main :: IO ()
main = print $ fF1 + fF2

-- ### MyLibrary.Math.MyFunc1
fF1 :: Int
fF1 = 10

-- ### MyLibrary.Math.MyFunc2
fF2 :: Int
fF2 = 77
```

By default, conflicting function names are given a suffix, and the shortest one that keeps the bundle collision-free wins:

1. The alias of your own `qualified ... as` import (`fF1`)
2. The uppercase letters of the module name (`fMF`)
3. The module name itself (`fMyFunc1`)
4. The whole path of the module, flattened (`fMyLibraryMathMyFunc1`)

Operators cannot carry a suffix, so they always keep their name. Make sure they have unique names, or use `--rename-cmd` to resolve it.

### Import unification

Imports in your submission file and your local libraries in use will be unified. This can cause some troubles.

For instance, one of your modules may hide some of the items it imports:

```haskell
-- MyLibrary/Mo.hs
import Prelude hiding (sort)

sort :: Int -> [(Int, Int)] -> [(Int, Int)]
sort = {- ... -}
```

The bundle also emits `import Prelude hiding (sort)`, and it may conflict with your code that's using `sort` in the global scope. `bundler-hs` does not resolve such conflicts, and your library must be written to avoid them.

### Language extension unification

`bundler-hs` emits the union of the `LANGUAGE` pragmas in every file and the `default-language` / `default-extensions` of its cabal project. Conflicting combinations can fail to compile.

### CPP

The `CPP` extension is handled separately from the pragma union. The CPP directives are preserved into the bundle, while the declarations inside every branch are renamed as usual:

```haskell
-- Expanded from `MyLibrary/Debug.hs`:
#ifdef DEBUG
debugD :: Bool
debugD = True
#else
debugD :: Bool
debugD = False
#endif
```

The support of `CPP` is very limited, and the above is the only expected use case.

### Tree shaking

You can use the tree-shaking options to drop unused code from your bundled output:

- `--tree-shake-lib` keeps only the library declarations your code actually reaches.
- `--tree-shake-app` does the same for your solution file.
- `--tree-shake` turns on both of them.

```sh
bundler-hs Main.hs --lib path/to/your/library --tree-shake-lib > submission.hs
```

- An `instance` is removed only when a local class or type of its head goes (an orphan instance is always kept).
- CPP conditionals are analysed with every branch, so a declaration only one branch uses survives.

### Minification

You can shrink the bundle when the judge limits the source size, or to shrink non-solution part of your submission:

- `--minify-lib` shrinks the bundled library into one module, dropping comments.
- `--minify-app` does the same to your own declarations.
- `--minify-import` puts the whole import section on one line.
- `--minify-language-extensions` combines every `LANGUAGE` pragma into one `{-# LANGUAGE A, B, ... #-}` line.
- `--minify` turns on all of them but `--minify-app`.

```sh
bundler-hs Main.hs --lib path/to/your/library --minify > submission.hs
```

### Formatting

`bundler-hs` uses `ghc-lib-parser` to recognize your code and operate on the AST for renaming etc., and then generates the bundled code from it. Therefore, the original format of your code will not be preserved.

The output is formatted with [hindent](https://github.com/mihaimaruseac/hindent) by default. Use the `--format-cmd` option to substitute another formatter.

> [ormolu](https://github.com/tweag/ormolu) does not work as expected, because the AST does not preserve your newlines.

## Limitations

Formatting is not preserved, as described in the `Formatting` section.

The generated code may not compile or run correctly even if your original code is correct. Basically, your imports and language extensions must be additive, and they must not conflict with each other. An open import (e.g., `import Data.List`) often conflicts with other import. Test with your library before contests!

## Development

Inside the dev shell (`direnv allow` or `nix develop`):

```console
$ just build          # cabal build all
$ just test           # golden test suite
$ just test-compile   # golden suite + ghc -fno-code check of every bundle
$ just test-accept    # re-record goldens after an intentional change
$ just run Main.hs --lib lib
```
