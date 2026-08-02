module Bundler.Config
  ( Config (..),
    EmbedPosition (..),
    FormatMode (..),
    MinifyOptions (..),
    anyMinify,
    noMinify,
    configParserInfo,
    parseConfigFromArgs,
  )
where

import Options.Applicative
import Options.Applicative.Help.Pretty (Doc, pretty, vsep)

data Config = Config
  { -- | The user's source file to bundle.
    cfgInput :: FilePath,
    -- | Roots under which local library modules are looked up
    -- (@--src DIR@, repeatable). An import @A.B.C@ is expanded iff
    -- @DIR/A/B/C.hs@ exists under one of these.
    cfgSrcDirs :: [FilePath],
    -- | Optional external command implementing the rename protocol
    -- (@--rename-cmd CMD@).
    cfgRenameCmd :: Maybe String,
    -- | How the finished bundle is formatted before printing.
    cfgFormat :: FormatMode,
    -- | Which sections of the bundle are minified (after formatting).
    cfgMinify :: MinifyOptions,
    -- | Where the expanded library code goes relative to the user's own
    -- declarations.
    cfgEmbedPosition :: EmbedPosition
  }
  deriving (Show)

-- | Placement of expanded library modules in the bundle. Haskell is
-- order-independent at the top level, so this is purely cosmetic.
data EmbedPosition
  = -- | Library code after the user's declarations (the default: your own
    -- code stays at the top of the submission).
    EmbedAfter
  | -- | Library code before the user's declarations, dependency-first.
    EmbedBefore
  deriving (Show)

-- | What happens to the bundle after assembly.
data FormatMode
  = -- | Builtin hindent (the default): every line break is re-decided, so
    -- the GHC pretty-printer's layout never leaks into the output.
    FormatBuiltin
  | -- | Pipe through an external command (stdin to stdout), e.g.
    -- @--format-cmd \'ormolu --stdin-input-file Bundle.hs\'@.
    FormatCmd String
  | -- | Emit the raw pretty-printer output (@--no-format@).
    FormatNone
  deriving (Show)

-- | Which sections of the bundle are minified. Applied after the
-- formatting stage, so non-minified sections keep their formatting; the
-- usual choice is minifying only the expanded library code.
data MinifyOptions = MinifyOptions
  { -- | Expanded library code: one layout-free line per declaration.
    moLib :: Bool,
    -- | The user's own declarations.
    moUser :: Bool,
    -- | The import section.
    moImports :: Bool,
    -- | Combine all LANGUAGE pragmas into a single line.
    moPragmas :: Bool
  }
  deriving (Show)

noMinify :: MinifyOptions
noMinify = MinifyOptions False False False False

anyMinify :: MinifyOptions -> Bool
anyMinify o = moLib o || moUser o || moImports o || moPragmas o

configParser :: Parser Config
configParser =
  Config
    <$> strArgument
      ( metavar "FILE"
          <> help "Haskell source file to bundle"
      )
    <*> many
      ( strOption
          ( long "src"
              <> metavar "DIR"
              <> help "Source directory of local library modules (repeatable)"
          )
      )
    <*> optional
      ( strOption
          ( long "rename-cmd"
              <> metavar "CMD"
              <> help "External command deciding renamed names (protocol at the bottom of this help)"
          )
      )
    <*> formatMode
    <*> minifyOptions
    <*> option
      readEmbedPosition
      ( long "embed-position"
          <> metavar "after|before"
          <> value EmbedAfter
          <> help "Where expanded library code goes relative to your own (default: after)"
      )

readEmbedPosition :: ReadM EmbedPosition
readEmbedPosition = eitherReader $ \s -> case s of
  "after" -> Right EmbedAfter
  "before" -> Right EmbedBefore
  _ -> Left ("expected 'after' or 'before', got " <> show s)

formatMode :: Parser FormatMode
formatMode =
  ( FormatCmd
      <$> strOption
        ( long "format-cmd"
            <> metavar "CMD"
            <> help
              "Format with a shell command (stdin to stdout)"
        )
  )
    <|> flag'
      FormatNone
      ( long "no-format"
          <> help "Emit the raw pretty-printer output without formatting"
      )
    <|> pure FormatBuiltin

minifyOptions :: Parser MinifyOptions
minifyOptions =
  combine
    <$> switch
      ( long "minify"
          <> help "Minify everything except your own code (shorthand)"
      )
    <*> switch
      ( long "minify-lib"
          <> help "Minify the expanded library code into one layout-free line (comments dropped)"
      )
    <*> switch
      ( long "minify-user-code"
          <> help "Minify your own declarations too"
      )
    <*> switch
      ( long "minify-import"
          <> help "Minify the import section into one line"
      )
    <*> switch
      ( long "minify-language-extensions"
          <> help "Combine all LANGUAGE pragmas into a single {-# LANGUAGE A, B, ... #-} line"
      )
  where
    combine everything lib user imports pragmas
      | everything = MinifyOptions True user True True
      | otherwise = MinifyOptions lib user imports pragmas

configParserInfo :: ParserInfo Config
configParserInfo =
  info
    (configParser <**> helper)
    ( fullDesc
        <> progDesc "Expand local library imports into one self-contained Haskell file on stdout"
        <> header "bundler-hs - Haskell source bundler for competitive programming"
        <> footerDoc (Just footerNotes)
    )

-- | Minification notes and the --rename-cmd protocols, shown at the bottom of --help. Keep the
-- rename part in sync with 'Bundler.RenameCmd.RenameQuery'.
footerNotes :: Doc
footerNotes =
  vsep
    ( map
        pretty
        [ "--rename-cmd is queried in lockstep via stdin/stdout, one line per name:",
          "",
          "    kind <TAB> module <TAB> default-suffix <TAB> name  ->  new-name",
          "",
          "  kind is value, type, con, field, op, or extmod.",
          "  `echo \"$name$suffix\"` reproduces the default behavior.",
          "",
          "  Example script:",
          "",
          "    #!/bin/sh",
          "    tab=$(printf '\\t')",
          "    while IFS=\"$tab\" read -r kind mod suffix name; do",
          "      case \"$mod\" in",
          "        SuffixArray) printf '%sSA\\n' \"$name\" ;;",
          "        *)           printf '%s%s\\n' \"$name\" \"$suffix\" ;;",
          "      esac",
          "    done"
        ]
    )

-- | Parse a raw argument list (used by the test harness; the executable
-- uses 'configParserInfo' with 'execParser' directly).
parseConfigFromArgs :: [String] -> Either String Config
parseConfigFromArgs args =
  case execParserPure defaultPrefs configParserInfo args of
    Success cfg -> Right cfg
    Failure failure -> Left (fst (renderFailure failure "bundler-hs"))
    CompletionInvoked _ -> Left "unexpected completion invocation"
