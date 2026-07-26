module Bundler.Render
  ( renderSDoc,
    renderModule,
    renderModuleHeader,
    renderImport,
    renderDecl,
  )
where

import GHC.Hs (GhcPs, HsModule (..), LHsDecl, LImportDecl)
import GHC.Types.SrcLoc (Located, unLoc)
import GHC.Utils.Outputable
  ( SDoc,
    defaultSDocContext,
    ppr,
    renderWithContext,
    sdocLineLength,
  )

-- | Render with a generous line length: the pretty-printer's wrapping is the
-- main source of hard-to-reparse output, so avoid it where possible.
renderSDoc :: SDoc -> String
renderSDoc = renderWithContext defaultSDocContext {sdocLineLength = 200}

-- | Render a whole parsed module (the @-ddump-parsed@ printer).
renderModule :: Located (HsModule GhcPs) -> String
renderModule = renderSDoc . ppr . unLoc

-- | Just the @module X (...) where@ header, rendered by printing the module
-- with its imports and declarations stripped. 'Nothing' for a headerless
-- file (implicit @Main@).
renderModuleHeader :: Located (HsModule GhcPs) -> Maybe String
renderModuleHeader lmod = case hsmodName m of
  Nothing -> Nothing
  Just _ -> Just (renderModule (fmap strip lmod))
  where
    m = unLoc lmod
    strip mm = mm {hsmodImports = [], hsmodDecls = []}

renderImport :: LImportDecl GhcPs -> String
renderImport = renderSDoc . ppr . unLoc

renderDecl :: LHsDecl GhcPs -> String
renderDecl = renderSDoc . ppr . unLoc
