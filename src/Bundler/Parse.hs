{-# LANGUAGE TypeApplications #-}

module Bundler.Parse
  ( ParsedFile (..),
    baseDynFlags,
    applyPragmaLines,
    nestingDelta,
    parseHaskellFile,
    parseUserFile,
  )
where

import Bundler.Error
import Control.Exception (SomeException, try)
import Data.Char (isAlpha, isAlphaNum, isSpace, toUpper)
import Data.List (dropWhileEnd, intercalate, isPrefixOf)
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
    -- | The file's header pragma lines (@LANGUAGE@, @OPTIONS_GHC@, ...),
    -- unioned into the bundle's pragma block.
    pfPragmas :: [String],
    -- | The whole header block above the module header - comments, blank
    -- lines, and pragmas in source order. Only the user file's is emitted
    -- (library headers are dropped, like the rest of their comments).
    pfHeader :: [String],
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

-- | Parse a library file: CPP directives, if any, are evaluated by cpphs
-- under the given macro definitions.
parseHaskellFile :: [(String, String)] -> DynFlags -> FilePath -> String -> IO (Either BundleError ParsedFile)
parseHaskellFile = parseWith CppEvaluate

-- | Parse the user's file: CPP directives between top-level declarations
-- are preserved into the bundle.
parseUserFile :: [(String, String)] -> DynFlags -> FilePath -> String -> IO (Either BundleError ParsedFile)
parseUserFile = parseWith CppPreserve

parseWith :: CppHandling -> [(String, String)] -> DynFlags -> FilePath -> String -> IO (Either BundleError ParsedFile)
parseWith cppMode userDefines dflags path rawSrc = do
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
      preprocessed <- try @SomeException (runCpphs (cpphsOptions userDefines) path src)
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
          pfHeader = extractHeader src,
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
-- would confuse the renamed bundle), the caller's macro definitions (from
-- @-D@ and from cabal @cpp-options@), and a __GLASGOW_HASKELL__ matching
-- the grammar ghc-lib-parser implements. Earlier definitions win, so the
-- caller can override the compiler version too.
cpphsOptions :: [(String, String)] -> CpphsOptions
cpphsOptions userDefines =
  defaultCpphsOptions
    { defines = dedupeOnKey (userDefines <> [("__GLASGOW_HASKELL__", "912")]),
      boolopts = defaultBoolOptions {locations = False}
    }
  where
    dedupeOnKey = go []
      where
        go _ [] = []
        go seen ((k, v) : rest)
          | k `elem` seen = go seen rest
          | otherwise = (k, v) : go (k : seen) rest

renderPsErrors :: PState -> String
renderPsErrors st =
  unlines
    [ render (ppr (errMsgSpan e) <+> vcat (unDecorated (diagnosticMessage opts (errMsgDiagnostic e))))
    | e <- bagToList (getMessages (getPsErrorMessages st))
    ]
  where
    opts = defaultDiagnosticOpts @PsMessage
    render = O.renderWithContext O.defaultSDocContext

-- | The file's header block for verbatim re-emission: one entry per logical
-- item - comment lines, block comments, blank lines, and header pragmas - in
-- source order. A multi-line block comment or pragma is one entry.
extractHeader :: String -> [String]
extractHeader = dropWhileEnd null . scanHeader False

-- | The header pragma lines only. CPP directives are stepped over here, so a
-- @{-\# LANGUAGE ... \#-}@ guarded by @#if@ still reaches the bundle's
-- pragma union (the branch is not evaluated: pragmas are additive anyway).
extractHeaderPragmas :: String -> [String]
extractHeaderPragmas = filter isPragmaItem . scanHeader True

-- | Walk the leading header, stopping at the first line that starts real
-- code. A declaration pragma such as @{-\# INLINE f \#-}@ counts as real
-- code: in a file without a module header those sit at column 1 too, and
-- lifting them into the bundle's header would detach them from what they
-- annotate.
--
-- @skipDirectives@ steps over CPP @#@ lines instead of stopping there; they
-- are never returned as items either way.
scanHeader :: Bool -> String -> [String]
scanHeader skipDirectives = go . lines
  where
    go [] = []
    go ls@(l : rest)
      | null trimmed = "" : go rest
      | "--" `isPrefixOf` trimmed = l : go rest
      | "{-#" `isPrefixOf` trimmed = if isHeaderPragma trimmed then block ls else []
      | "{-" `isPrefixOf` trimmed = block ls
      | skipDirectives, "#" `isPrefixOf` l = go rest
      | otherwise = []
      where
        trimmed = dropWhile (== ' ') l

    -- One comment or pragma, which may span lines; nesting is tracked so a
    -- multi-line {- ... -} does not swallow the rest of the file.
    block ls = intercalate "\n" (reverse taken) : go leftover
      where
        (taken, leftover) = spanNested 0 [] ls
        spanNested _ acc [] = (acc, [])
        spanNested depth acc (x : xs)
          | depth' <= 0 = (x : acc, xs)
          | otherwise = spanNested depth' (x : acc) xs
          where
            depth' = depth + nestingDelta x

    -- Only these belong above the module header; anything else in {-# #-}
    -- annotates a declaration.
    isHeaderPragma t =
      map toUpper (takeWhile (\c -> isAlphaNum c || c == '_') (dropWhile isSpace (drop 3 t)))
        `elem` ["LANGUAGE", "OPTIONS", "OPTIONS_GHC", "OPTIONS_HADDOCK", "INCLUDE"]

isPragmaItem :: String -> Bool
isPragmaItem l = "{-#" `isPrefixOf` dropWhile (== ' ') l

-- | Open minus close comment brackets on one line. @{-\#@ and @\#-}@ are
-- ordinary brackets as far as nesting goes, so pragmas balance out.
nestingDelta :: String -> Int
nestingDelta = go 0
  where
    go n ('{' : '-' : r) = go (n + 1) r
    go n ('-' : '}' : r) = go (n - 1) r
    go n (_ : r) = go n r
    go n [] = n
