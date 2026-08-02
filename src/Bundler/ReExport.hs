-- | Resolve @module M@ export items across local modules. Pure symbol
-- bookkeeping between discovery and renaming: for every local module, in
-- dependency order, compute which imported names its export list
-- re-exports and record their defining module, so importers can attribute
-- those names without following the chain themselves.
module Bundler.ReExport
  ( resolveReExports,
  )
where

import Bundler.Discovery
import Bundler.Error
import Bundler.Parse
import Bundler.Symbols
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import GHC.Hs (ImportDeclQualifiedStyle (..), hsmodImports, ideclAs, ideclName, ideclQualified)
import GHC.Types.SrcLoc (unLoc)
import Language.Haskell.Syntax.Module.Name (ModuleName, moduleNameString)

-- | Fill in 'msReExported' for every module. The input must be in
-- dependency order (as 'discoverLocalModules' produces): a re-export of a
-- re-export then resolves to its original definer in one sweep.
--
-- @module M@ exports every name in scope both unqualified and qualified as
-- @M.x@; we honor its common form - non-qualified imports whose qualifier
-- (alias or module name) is @M@. A @module M@ over an external module
-- cannot be followed without package interfaces: its names simply stay
-- unattributed, exactly as before this pass existed.
resolveReExports ::
  [(LocalModule, ModuleSymbols)] ->
  Either BundleError [(LocalModule, ModuleSymbols)]
resolveReExports = go Map.empty []
  where
    go _ acc [] = Right (reverse acc)
    go resolved acc ((lm, syms) : rest) = do
      reExported <- reExportsOf resolved lm syms
      let syms' = syms {msReExported = reExported}
      go (Map.insert (lmName lm) syms' resolved) ((lm, syms') : acc) rest

    reExportsOf ::
      Map ModuleName ModuleSymbols ->
      LocalModule ->
      ModuleSymbols ->
      Either BundleError (Map OccKey ModuleName)
    reExportsOf resolved lm syms =
      Map.unions
        <$> sequence
          [ either (notExported m) Right (importVisibleOrigins (`Map.lookup` resolved) m impSyms decl)
          | target <- msReExportedModules syms,
            decl <- map unLoc (hsmodImports (unLoc (pfModule (lmParsed lm)))),
            let m = unLoc (ideclName decl),
            maybe m unLoc (ideclAs decl) == target,
            ideclQualified decl == NotQualified,
            Just impSyms <- [Map.lookup m resolved]
          ]
      where
        notExported m name = Left (NotExported name (moduleNameString m))
