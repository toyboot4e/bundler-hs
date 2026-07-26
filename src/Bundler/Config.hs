module Bundler.Config
  ( Config (..),
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
    cfgRenameCmd :: Maybe String
  }
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
