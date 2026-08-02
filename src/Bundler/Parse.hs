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
    srcSpanEndLine,
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
    -- | Preserved CPP directive lines of the user's file, as
    -- @(declaration index, original line number, text)@: each directive is
    -- anchored to the index of the top-level declaration it precedes (an
    -- index equal to the number of declarations means \"after the last
    -- one\"). Always empty for library files, whose directives are
    -- evaluated instead.
    pfDirectives :: [(Int, Int, String)],
    -- | The source text the declaration spans refer to: the original file,
    -- except when CPP was evaluated (then the preprocessed text). For the
    -- directive-preserving parse the original still lines up, because
    -- directives were only blanked in place and never cut a declaration.
    pfSource :: String
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
parseWith cppMode dflags path rawSrc = do
  mflags <- parsePragmasIntoDynFlags dflags ([], []) path src
  case mflags of
    Left err ->
      pure (Left (ParseError path err))
    Right flags ->
      case parseFile path flags src of
        POk _ modl -> pure (Right (mkParsed flags modl [] src))
        -- A file that merely enables CPP but contains no # directives is
        -- ordinary Haskell and parses directly. Real directives make the
        -- raw parse fail and are handled per 'CppHandling'.
        PFailed st
          | xopt LangExt.Cpp flags -> case cppMode of
              CppPreserve
                | Just (stripped, directives) <- stripDirectives src,
                  POk _ modl <- parseFile path flags stripped,
                  Just anchored <- anchorDirectives modl directives ->
                    pure (Right (mkParsed flags modl anchored src))
              _ -> evaluateCpp flags
          | otherwise -> pure (Left (ParseError path (renderPsErrors st)))
  where
    src = normalizeNewlines rawSrc

    evaluateCpp flags = do
      preprocessed <- try @SomeException (runCpphs cpphsOptions path src)
      pure $ case preprocessed of
        Left err ->
          Left (ParseError path ("CPP preprocessing failed: " <> show err))
        Right src' -> case parseFile path flags src' of
          POk _ modl -> Right (mkParsed flags modl [] src')
          PFailed st' -> Left (ParseError path (renderPsErrors st'))

    -- Header pragmas are taken from the original source: the LANGUAGE
    -- pragmas must survive into the bundle even when the declarations come
    -- from preprocessed or directive-stripped text.
    mkParsed flags modl directives spanSrc =
      ParsedFile
        { pfPath = path,
          pfModule = modl,
          pfDynFlags = flags,
          pfPragmas = extractHeaderPragmas src,
          pfDirectives = directives,
          pfSource = spanSrc
        }

-- | CRLF-tolerant reading: the bundle is always emitted with plain LF, and
-- stray carriage returns would otherwise survive inside re-emitted pragma
-- and directive lines.
normalizeNewlines :: String -> String
normalizeNewlines ('\r' : '\n' : rest) = '\n' : normalizeNewlines rest
normalizeNewlines (c : rest) = c : normalizeNewlines rest
normalizeNewlines [] = []

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
-- 'Nothing' when any directive falls *inside* a declaration's span: naively
-- blanking such a line changes meaning (e.g. an @#ifdef DEBUG@ statement in
-- a do-block would become unconditional), so the caller must evaluate the
-- CPP instead of preserving it.
anchorDirectives :: Located (HsModule GhcPs) -> [(Int, String)] -> Maybe [(Int, Int, String)]
anchorDirectives modl directives
  | all betweenDecls directives =
      Just
        [ (length (filter ((< line) . fst) declSpans), line, text)
        | (line, text) <- directives
        ]
  | otherwise = Nothing
  where
    betweenDecls (line, _) =
      all (\(start, end) -> line < start || line > end) declSpans
    declSpans =
      [ (srcSpanStartLine real, srcSpanEndLine real)
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
