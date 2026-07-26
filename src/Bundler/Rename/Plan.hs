module Bundler.Rename.Plan
  ( RenamePlan (..)
  , mkRenamePlan
  , planSuffixFor
  ) where

import Bundler.Discovery
import Bundler.Error
import Bundler.Parse
import Bundler.Symbols
import Data.Char (isAlphaNum)
import Data.List (sort)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (listToMaybe, mapMaybe)
import GHC.Hs (ideclAs, ideclName, hsmodImports)
import GHC.Types.SrcLoc (unLoc)
import Language.Haskell.Syntax.Module.Name (ModuleName, moduleNameString)

-- | For every local module, the new (always unqualified) name of each of its
-- top-level names.
newtype RenamePlan = RenamePlan
  { rpByModule :: Map ModuleName (Map OccKey String)
  }
  deriving (Show)

-- | The suffix appended to every name of a module: the @as@ alias from the
-- user's own import of it when present, otherwise the dot-stripped module
-- name. (@--rename-cmd@ overrides this per name; wired up in M6.)
planSuffixFor :: ParsedFile -> ModuleName -> String
planSuffixFor userFile m =
  maybe (filter (/= '.') (moduleNameString m)) moduleNameString userAlias
  where
    userAlias =
      listToMaybe . mapMaybe aliasOf . map unLoc . hsmodImports . unLoc $
        pfModule userFile
    aliasOf imp
      | unLoc (ideclName imp) == m = fmap unLoc (ideclAs imp)
      | otherwise = Nothing

-- | Build the default plan and validate that the resulting flat namespace
-- has no collisions (including against the user's own top-level names).
mkRenamePlan
  :: ParsedFile
  -> ModuleSymbols
  -- ^ The user file's own symbols (unrenamed, but they occupy names).
  -> [(LocalModule, ModuleSymbols)]
  -> Either BundleError RenamePlan
mkRenamePlan userFile userSyms locals = do
  validate
  pure plan
  where
    plan =
      RenamePlan . Map.fromList $
        [ (lmName lm, Map.mapWithKey (newName (suffix lm)) (msAll syms))
        | (lm, syms) <- locals
        ]
    suffix lm = planSuffixFor userFile (lmName lm)

    -- Operators cannot take an alphanumeric suffix; they keep their name
    -- (collisions caught below, resolvable via --rename-cmd once wired).
    newName suf (_, name) _kind
      | isOperatorName name = name
      | otherwise = name <> suf

    isOperatorName = not . all (\c -> isAlphaNum c || c `elem` "_'")

    -- Flat-namespace collision check: every renamed name, per namespace
    -- bucket, plus the user file's own top-level names.
    validate =
      case Map.toAscList collisions of
        [] -> Right ()
        ((ns, name), origins) : _ ->
          Left (NameCollision (describe ns name) (sort origins))
    collisions =
      Map.filter (\os -> length os > 1) . Map.fromListWith (<>) $
        [ ((ns, new), [moduleNameString (lmName lm)])
        | (lm, syms) <- locals
        , ((ns, old), _) <- Map.toList (msAll syms)
        , let new = if isOperatorName old then old else old <> suffix lm
        ]
          <> [ ((ns, name), ["<user file>"])
             | (ns, name) <- Map.keys (msAll userSyms)
             ]
    describe ns name = case ns of
      NsValue -> name
      NsData -> name <> " (constructor)"
      NsTcCls -> name <> " (type)"
