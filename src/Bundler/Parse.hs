{-# LANGUAGE TypeApplications #-}

module Bundler.Parse
  ( ParsedFile (..)
  , baseDynFlags
  , parseHaskellFile
  ) where

import Bundler.Error
import GHC.Data.Bag (bagToList)
import GHC.Driver.Session (DynFlags, defaultDynFlags)
import GHC.Hs (GhcPs, HsModule)
import GHC.Parser.Errors.Types (PsMessage)
import GHC.Parser.Lexer (PState, ParseResult (..), getPsErrorMessages)
import GHC.Types.Error
  ( MsgEnvelope (..)
  , defaultDiagnosticOpts
  , diagnosticMessage
  , getMessages
  , unDecorated
  )
import GHC.Types.SrcLoc (Located)
import GHC.Utils.Outputable (ppr, vcat, (<+>))
import qualified GHC.Utils.Outputable as O
import Language.Haskell.GhclibParserEx.GHC.Driver.Session (parsePragmasIntoDynFlags)
import Language.Haskell.GhclibParserEx.GHC.Parser (parseFile)
import Language.Haskell.GhclibParserEx.GHC.Settings.Config (fakeSettings)

-- | One successfully parsed source file, together with the flags it was
-- parsed under (needed again for the output self-check) and its raw header
-- pragma lines (re-emitted verbatim into the bundle).
data ParsedFile = ParsedFile
  { pfPath :: FilePath
  , pfModule :: Located (HsModule GhcPs)
  , pfDynFlags :: DynFlags
  , pfPragmas :: [String]
  }

-- | Flags before any per-project or per-file additions.
baseDynFlags :: DynFlags
baseDynFlags = defaultDynFlags fakeSettings

-- | Parse one file: apply its own LANGUAGE/OPTIONS_GHC pragmas on top of the
-- given base flags, then parse.
parseHaskellFile :: DynFlags -> FilePath -> String -> IO (Either BundleError ParsedFile)
parseHaskellFile dflags path src = do
  mflags <- parsePragmasIntoDynFlags dflags ([], []) path src
  case mflags of
    Left err ->
      pure (Left (ParseError path err))
    Right flags ->
      pure $ case parseFile path flags src of
        POk _ modl ->
          Right
            ParsedFile
              { pfPath = path
              , pfModule = modl
              , pfDynFlags = flags
              , pfPragmas = extractHeaderPragmas src
              }
        PFailed st -> Left (ParseError path (renderPsErrors st))

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
