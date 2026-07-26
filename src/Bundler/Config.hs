module Bundler.Config
  ( Config (..)
  , configParserInfo
  , parseConfigFromArgs
  ) where

import Options.Applicative

data Config = Config
  { cfgInput :: FilePath
  -- ^ The user's source file to bundle.
  , cfgSrcDirs :: [FilePath]
  -- ^ Roots under which local library modules are looked up
  -- (@--src DIR@, repeatable). An import @A.B.C@ is expanded iff
  -- @DIR/A/B/C.hs@ exists under one of these.
  , cfgRenameCmd :: Maybe String
  -- ^ Optional external command implementing the rename protocol
  -- (@--rename-cmd CMD@).
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
    )

-- | Parse a raw argument list (used by the test harness; the executable
-- uses 'configParserInfo' with 'execParser' directly).
parseConfigFromArgs :: [String] -> Either String Config
parseConfigFromArgs args =
  case execParserPure defaultPrefs configParserInfo args of
    Success cfg -> Right cfg
    Failure failure -> Left (fst (renderFailure failure "bundler-hs"))
    CompletionInvoked _ -> Left "unexpected completion invocation"
