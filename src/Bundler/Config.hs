module Bundler.Config
  ( Config (..),
    EmbedPosition (..),
    FormatMode (..),
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
  | -- | Emit a pragma block plus one layout-free line (@--minify@).
    FormatMinify
  deriving (Show)

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
              <> help "External command deciding renamed names (TSV protocol on stdin/stdout)"
          )
      )
    <*> formatMode
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
              "Format with a shell command (stdin to stdout) instead of the builtin hindent, e.g. 'ormolu --stdin-input-file Bundle.hs'"
        )
  )
    <|> flag'
      FormatNone
      ( long "no-format"
          <> help "Emit the raw pretty-printer output without formatting"
      )
    <|> flag'
      FormatMinify
      ( long "minify"
          <> help "Emit the bundle as a pragma block plus one layout-free line"
      )
    <|> pure FormatBuiltin

configParserInfo :: ParserInfo Config
configParserInfo =
  info
    (configParser <**> helper)
    ( fullDesc
        <> progDesc "Expand local library imports into one self-contained Haskell file on stdout"
        <> header "bundler-hs - Haskell source bundler for competitive programming"
        <> footerDoc (Just renameCmdProtocol)
    )

-- | The --rename-cmd wire protocol, shown at the bottom of --help. Keep in
-- sync with 'Bundler.RenameCmd.RenameQuery'.
renameCmdProtocol :: Doc
renameCmdProtocol =
  vsep
    ( map
        pretty
        [ "The --rename-cmd protocol:",
          "  CMD is started once. For every name, one tab-separated query line is",
          "  written to its stdin and one response line is read from its stdout:",
          "",
          "    kind <TAB> module <TAB> default-suffix <TAB> name  ->  new-name",
          "",
          "  kind is value, type, con, field, op, or extmod. All fields are",
          "  non-empty; default-suffix is what the default rule would append, so",
          "  `echo \"$name$suffix\"` reproduces the default behavior.",
          "",
          "  op:     the name is an operator; the response must be symbolic too.",
          "  extmod: module/name are an external module; the response is the",
          "          qualifier to import it under in the bundle.",
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
