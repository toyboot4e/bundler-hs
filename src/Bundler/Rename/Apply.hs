module Bundler.Rename.Apply
  ( ResolveEnv (..)
  , mkResolveEnv
  , applyRenames
  ) where

import Bundler.Error
import Bundler.Parse
import Bundler.Rename.Plan
import Bundler.Symbols
import Data.Generics (everywhereM, mkM)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import GHC.Hs (GhcPs, LHsDecl, hsmodImports, ideclAs, ideclName)
import GHC.Types.Name.Occurrence (mkOccName, occNameSpace, occNameString)
import GHC.Types.Name.Reader (RdrName (..), mkRdrUnqual, rdrNameOcc)
import GHC.Types.SrcLoc (unLoc)
import Language.Haskell.Syntax.Module.Name (ModuleName, moduleNameString)

-- | How names written in one particular file resolve to local modules.
data ResolveEnv = ResolveEnv
  { reSelf :: Maybe ModuleName
  -- ^ The module the rewritten file itself defines ('Nothing' for the
  -- user's file, whose own names are never renamed).
  , reQualLocal :: Map ModuleName ModuleName
  -- ^ Written qualifier (alias or module name) -> local module.
  }

-- | Build the environment for one file from its imports of local modules.
-- A non-qualified import also allows qualified access under the same name,
-- so every local import contributes its qualifier.
mkResolveEnv :: Set ModuleName -> Maybe ModuleName -> ParsedFile -> ResolveEnv
mkResolveEnv localNames self pf =
  ResolveEnv
    { reSelf = self
    , reQualLocal =
        Map.fromList . mapMaybe (qualOf . unLoc) . hsmodImports . unLoc $
          pfModule pf
    }
  where
    qualOf imp
      | m `Set.member` localNames =
          Just (maybe m unLoc (ideclAs imp), m)
      | otherwise = Nothing
      where
        m = unLoc (ideclName imp)

-- | Rewrite every 'RdrName' in the given declarations according to the
-- plan. Namespace-aware by construction: the parser has already put each
-- occurrence in the right 'OccName' namespace, and lookups go through the
-- same 'OccKey' encoding used when collecting binders.
--
-- M2 scope: definition sites, own-module references, and qualified
-- references. Unqualified imports and shadowing arrive with M3's scope
-- tracking.
applyRenames
  :: RenamePlan
  -> ResolveEnv
  -> [LHsDecl GhcPs]
  -> Either BundleError [LHsDecl GhcPs]
applyRenames plan env = traverse (everywhereM (mkM rewrite))
  where
    rewrite :: RdrName -> Either BundleError RdrName
    rewrite rdr = case rdr of
      Unqual occ
        | Just self <- reSelf env
        , Just new <- lookupPlan self (occKeyOf rdr) ->
            Right (unqual occ new)
      Qual q occ
        | Just m <- Map.lookup q (reQualLocal env) ->
            case lookupPlan m (occKeyOf (Unqual occ)) of
              Just new -> Right (unqual occ new)
              Nothing ->
                Left
                  ( UnknownQualifiedName
                      (moduleNameString q <> "." <> occNameString occ)
                      (moduleNameString m)
                  )
      _ -> Right rdr

    lookupPlan m key =
      Map.lookup m (rpByModule plan) >>= Map.lookup key

    -- Rebuild in the original occurrence's namespace so a renamed type
    -- constructor stays a type name, a data constructor a data name, etc.
    unqual occ new = mkRdrUnqual (mkOccName (occNameSpace occ) new)
