module Bundler.Error
  ( BundleError (..),
    renderBundleError,
  )
where

import Data.List (intercalate)

-- | Every way bundling can fail. Rendered to stderr; the process exits
-- non-zero on any of these. Deliberately GHC-free (plain 'String's) so error
-- rendering stays trivial and deterministic.
data BundleError
  = -- | @file@ failed to parse; the second field is the rendered GHC
    -- diagnostic block.
    ParseError FilePath String
  | -- | The bundler's own output failed to re-parse. Always a bundler bug;
    -- the second field is the offending output for diagnosis.
    SelfCheckError String String
  | -- | A file enables CPP, which ghc-lib-parser cannot preprocess.
    CppNotSupported FilePath
  | -- | A local module resolves to files under more than one @--src@ dir.
    DuplicateModule String [FilePath]
  | -- | Local modules import each other in a cycle.
    ImportCycle [String]
  | -- | A @.cabal@ file exists but does not parse.
    CabalError FilePath String
  | -- | More than one @.cabal@ file in the project root candidate.
    AmbiguousCabal FilePath [FilePath]
  | -- | Two renamed names (or a renamed name and a user-file name) collide
    -- in the bundle's flat namespace.
    NameCollision String [String]
  | -- | A qualified reference to a local module names something that
    -- module does not define (likely a re-export, unsupported in v1).
    UnknownQualifiedName String String
  | -- | An unqualified name is importable from several local modules.
    AmbiguousName String [String]
  | -- | An import list mentions a name the local module does not export.
    NotExported String String
  | -- | The @--rename-cmd@ child failed or produced an unusable response.
    RenameCmdError String String
  deriving (Show)

renderBundleError :: BundleError -> String
renderBundleError err = case err of
  ParseError path msg ->
    "error: failed to parse " <> path <> ":\n" <> msg
  SelfCheckError msg output ->
    unlines
      [ "internal error: the generated bundle does not re-parse.",
        "This is a bug in haskell-source-bundler; please report it.",
        "",
        msg,
        "",
        "--- generated output ---",
        output
      ]
  CppNotSupported path ->
    "error: " <> path <> " uses CPP, which the bundler cannot preprocess"
  DuplicateModule name paths ->
    unlines
      ( ("error: module " <> name <> " is found under more than one --src directory:")
          : map ("  - " <>) paths
      )
  ImportCycle names ->
    "error: local modules form an import cycle: "
      <> intercalate " -> " (names <> take 1 names)
  CabalError path msg ->
    "error: failed to parse " <> path <> ": " <> msg
  AmbiguousCabal dir files ->
    unlines
      ( ("error: multiple .cabal files in " <> dir <> ":")
          : map ("  - " <>) files
      )
  NameCollision name origins ->
    unlines
      ( ("error: renamed name " <> name <> " collides between:")
          : map ("  - " <>) origins
            <> ["hint: use --rename-cmd to pick different names"]
      )
  UnknownQualifiedName written m ->
    "error: "
      <> written
      <> " does not resolve to a definition in local module "
      <> m
      <> " (re-exports are not supported)"
  AmbiguousName name origins ->
    "error: "
      <> name
      <> " is ambiguous; it could come from "
      <> intercalate " or " origins
  NotExported name m ->
    "error: module " <> m <> " does not export " <> name
  RenameCmdError ctx why ->
    "error: rename command failed (" <> ctx <> "): " <> why
