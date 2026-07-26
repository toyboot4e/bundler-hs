{-# LANGUAGE TypeApplications #-}

module Bundler.Parse
  ( ParsedFile (..),
    baseDynFlags,
    applyPragmaLines,
    parseHaskellFile,
  )
where

import Bundler.Error
import Control.Exception (SomeException, try)
import GHC.Data.Bag (bagToList)
import GHC.Driver.Session (DynFlags, defaultDynFlags, xopt)
import GHC.Hs (GhcPs, HsModule)
import GHC.LanguageExtensions.Type qualified as LangExt
import GHC.Parser.Errors.Types (PsMessage)
import GHC.Parser.Lexer (PState, ParseResult (..), getPsErrorMessages)
import GHC.Types.Error
  ( MsgEnvelope (..),
    defaultDiagnosticOpts,
    diagnosticMessage,
    getMessages,
    unDecorated,
  )
import GHC.Types.SrcLoc (Located)
import GHC.Utils.Outputable (ppr, vcat, (<+>))
import GHC.Utils.Outputable qualified as O
import Language.Haskell.GhclibParserEx.GHC.Driver.Session (parsePragmasIntoDynFlags)
import Language.Haskell.GhclibParserEx.GHC.Parser (parseFile)
import Language.Haskell.GhclibParserEx.GHC.Settings.Config (fakeSettings)
import Language.Preprocessor.Cpphs
  ( BoolOptions (..),
    CpphsOptions (..),
    defaultBoolOptions,
    defaultCpphsOptions,
    runCpphs,
  )

-- | One successfully parsed source file, together with the flags it was
-- parsed under (needed again for the output self-check) and its raw header
-- pragma lines (re-emitted verbatim into the bundle).
data ParsedFile = ParsedFile
  { pfPath :: FilePath,
    pfModule :: Located (HsModule GhcPs),
    pfDynFlags :: DynFlags,
    pfPragmas :: [String]
  }

-- | Flags before any per-project or per-file additions.
baseDynFlags :: DynFlags
baseDynFlags = defaultDynFlags fakeSettings

-- | Apply synthesized @{-\# LANGUAGE ... \#-}@ lines (from cabal
-- @default-language@/@default-extensions@) on top of the given flags, using
-- GHC's own pragma parser so editions, @NoX@ negation, and implied
-- extensions behave exactly as in a source file.
applyPragmaLines :: DynFlags -> [String] -> IO (Either BundleError DynFlags)
applyPragmaLines dflags [] = pure (Right dflags)
applyPragmaLines dflags pragmaLines = do
  parsed <- parsePragmasIntoDynFlags dflags ([], []) "<cabal defaults>" (unlines pragmaLines)
  pure $ case parsed of
    Left err -> Left (CabalError "<cabal defaults>" err)
    Right flags -> Right flags

-- | Parse one file: apply its own LANGUAGE/OPTIONS_GHC pragmas on top of the
-- given base flags, then parse.
parseHaskellFile :: DynFlags -> FilePath -> String -> IO (Either BundleError ParsedFile)
parseHaskellFile dflags path src = do
  mflags <- parsePragmasIntoDynFlags dflags ([], []) path src
  case mflags of
    Left err ->
      pure (Left (ParseError path err))
    Right flags ->
      case parseFile path flags src of
        POk _ modl -> pure (Right (mkParsed flags modl src))
        -- A file that merely enables CPP but contains no # directives is
        -- ordinary Haskell and parses directly. Real directives make the
        -- raw parse fail; run cpphs and parse the preprocessed result.
        PFailed st
          | xopt LangExt.Cpp flags -> do
              preprocessed <- try @SomeException (runCpphs cpphsOptions path src)
              pure $ case preprocessed of
                Left err ->
                  Left (ParseError path ("CPP preprocessing failed: " <> show err))
                Right src' -> case parseFile path flags src' of
                  POk _ modl -> Right (mkParsed flags modl src)
                  PFailed st' -> Left (ParseError path (renderPsErrors st'))
          | otherwise -> pure (Left (ParseError path (renderPsErrors st)))
  where
    -- Header pragmas are taken from the original source: the LANGUAGE
    -- pragmas must survive into the bundle even when the declarations come
    -- from the preprocessed text.
    mkParsed flags modl original =
      ParsedFile
        { pfPath = path,
          pfModule = modl,
          pfDynFlags = flags,
          pfPragmas = extractHeaderPragmas original
        }

-- | cpphs setup for library code: no #line markers in the output (they
-- would confuse the renamed bundle), and a __GLASGOW_HASKELL__ matching the
-- grammar ghc-lib-parser implements.
cpphsOptions :: CpphsOptions
cpphsOptions =
  defaultCpphsOptions
    { defines = [("__GLASGOW_HASKELL__", "912")],
      boolopts = defaultBoolOptions {locations = False}
    }

renderPsErrors :: PState -> String
renderPsErrors st =
  unlines
    [ render (ppr (errMsgSpan e) <+> vcat (unDecorated (diagnosticMessage opts (errMsgDiagnostic e))))
    | e <- bagToList (getMessages (getPsErrorMessages st))
    ]
  where
    opts = defaultDiagnosticOpts @PsMessage
    render = O.renderWithContext O.defaultSDocContext

-- | Header pragma lines (@{-\# LANGUAGE ... \#-}@ etc.) before the module
-- header, re-emitted verbatim. Single-line pragmas only; multi-line header
-- pragmas are out of scope for now.
extractHeaderPragmas :: String -> [String]
extractHeaderPragmas =
  filter isPragma . takeWhile (not . isModuleStart) . lines
  where
    isPragma l = take 3 (dropWhile (== ' ') l) == "{-#"
    isModuleStart l = take 7 l == "module "
