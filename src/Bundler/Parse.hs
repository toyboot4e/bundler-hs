{-# LANGUAGE TypeApplications #-}

module Bundler.Parse
  ( ParsedFile (..),
    baseDynFlags,
    applyPragmaLines,
    parseHaskellFile,
    parseUserFile,
  )
where

import Bundler.Error
import Control.Exception (SomeException, try)
import Data.Char (isAlpha, isSpace)
import GHC.Data.Bag (bagToList)
import GHC.Driver.Session (DynFlags, defaultDynFlags, xopt)
import GHC.Hs (GhcPs, HsModule, getLocA, hsmodDecls)
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
import GHC.Types.SrcLoc
  ( Located,
    SrcSpan (..),
    srcSpanStartLine,
    unLoc,
  )
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
    pfPragmas :: [String],
    -- | Preserved CPP directive lines of the user's file, each anchored to
    -- the index of the top-level declaration it precedes (an index equal to
    -- the number of declarations means \"after the last one\"). Always
    -- empty for library files, whose directives are evaluated instead.
    pfDirectives :: [(Int, String)]
  }

-- | How to treat CPP @#@ directives when the raw parse fails.
data CppHandling
  = -- | Run cpphs and bundle the chosen branches (library modules; the
    -- renamer needs one coherent set of top-level names).
    CppEvaluate
  | -- | Keep directives that sit between top-level declarations, renaming
    -- all branches (user file). Falls back to 'CppEvaluate' when
    -- directives cut through the middle of a declaration.
    CppPreserve

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

-- | Parse a library file: CPP directives, if any, are evaluated by cpphs.
parseHaskellFile :: DynFlags -> FilePath -> String -> IO (Either BundleError ParsedFile)
parseHaskellFile = parseWith CppEvaluate

-- | Parse the user's file: CPP directives between top-level declarations
-- are preserved into the bundle.
parseUserFile :: DynFlags -> FilePath -> String -> IO (Either BundleError ParsedFile)
parseUserFile = parseWith CppPreserve

parseWith :: CppHandling -> DynFlags -> FilePath -> String -> IO (Either BundleError ParsedFile)
parseWith cppMode dflags path src = do
  mflags <- parsePragmasIntoDynFlags dflags ([], []) path src
  case mflags of
    Left err ->
      pure (Left (ParseError path err))
    Right flags ->
      case parseFile path flags src of
        POk _ modl -> pure (Right (mkParsed flags modl []))
        -- A file that merely enables CPP but contains no # directives is
        -- ordinary Haskell and parses directly. Real directives make the
        -- raw parse fail and are handled per 'CppHandling'.
        PFailed st
          | xopt LangExt.Cpp flags -> case cppMode of
              CppPreserve
                | Just (stripped, directives) <- stripDirectives src,
                  POk _ modl <- parseFile path flags stripped ->
                    pure (Right (mkParsed flags modl (anchorDirectives modl directives)))
              _ -> evaluateCpp flags
          | otherwise -> pure (Left (ParseError path (renderPsErrors st)))
  where
    evaluateCpp flags = do
      preprocessed <- try @SomeException (runCpphs cpphsOptions path src)
      pure $ case preprocessed of
        Left err ->
          Left (ParseError path ("CPP preprocessing failed: " <> show err))
        Right src' -> case parseFile path flags src' of
          POk _ modl -> Right (mkParsed flags modl [])
          PFailed st' -> Left (ParseError path (renderPsErrors st'))

    -- Header pragmas are taken from the original source: the LANGUAGE
    -- pragmas must survive into the bundle even when the declarations come
    -- from preprocessed or directive-stripped text.
    mkParsed flags modl directives =
      ParsedFile
        { pfPath = path,
          pfModule = modl,
          pfDynFlags = flags,
          pfPragmas = extractHeaderPragmas src,
          pfDirectives = directives
        }

-- | Replace CPP directive lines with blank ones (keeping line numbers
-- intact) and return them tagged with their line number. 'Nothing' when the
-- file has no directives at all. The stripped source parses only when every
-- directive sits between top-level declarations - the caller falls back to
-- evaluation otherwise.
stripDirectives :: String -> Maybe (String, [(Int, String)])
stripDirectives src
  | null directives = Nothing
  | otherwise = Just (unlines stripped, directives)
  where
    numbered = zip [1 :: Int ..] (lines src)
    directives = [(n, l) | (n, l) <- numbered, isDirective l]
    stripped = [if isDirective l then "" else l | (_, l) <- numbered]
    -- Column-1 hash followed by a CPP keyword (or a line marker number);
    -- anything else could be an operator and is left alone.
    isDirective ('#' : rest) =
      case dropWhile isSpace rest of
        kw -> takeWhile isAlpha kw `elem` cppKeywords || isLineMarker kw
    isDirective _ = False
    cppKeywords =
      [ "if",
        "ifdef",
        "ifndef",
        "elif",
        "else",
        "endif",
        "define",
        "undef",
        "include",
        "error",
        "warning",
        "line",
        "pragma"
      ]
    isLineMarker kw = take 1 kw `elem` ["0", "1", "2", "3", "4", "5", "6", "7", "8", "9"]

-- | Attach each directive to the index of the first top-level declaration
-- starting after it (so it is re-emitted just before that declaration).
anchorDirectives :: Located (HsModule GhcPs) -> [(Int, String)] -> [(Int, String)]
anchorDirectives modl directives =
  [ (length (filter (< line) declStartLines), text)
  | (line, text) <- directives
  ]
  where
    declStartLines =
      [ srcSpanStartLine real
      | decl <- hsmodDecls (unLoc modl),
        RealSrcSpan real _ <- [getLocA decl]
      ]

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
