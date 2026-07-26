module Bundler.Render
  ( renderSDoc
  , renderModule
  ) where

import GHC.Hs (GhcPs, HsModule)
import GHC.Types.SrcLoc (Located, unLoc)
import GHC.Utils.Outputable
  ( SDoc
  , defaultSDocContext
  , ppr
  , renderWithContext
  , sdocLineLength
  )

-- | Render with a generous line length: the pretty-printer's wrapping is the
-- main source of hard-to-reparse output, so avoid it where possible.
renderSDoc :: SDoc -> String
renderSDoc = renderWithContext defaultSDocContext {sdocLineLength = 200}

-- | Render a whole parsed module (the @-ddump-parsed@ printer).
renderModule :: Located (HsModule GhcPs) -> String
renderModule = renderSDoc . ppr . unLoc
