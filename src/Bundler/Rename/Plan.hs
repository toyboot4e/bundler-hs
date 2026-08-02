module Bundler.Rename.Plan
  ( RenamePlan (..),
    mkRenamePlan,
    planSuffixFor,
  )
where

import Bundler.Discovery
import Bundler.Error
import Bundler.Parse
import Bundler.RenameCmd
import Bundler.Symbols
import Control.Monad.Trans.Except (ExceptT (..), runExceptT)
import Data.List (sort)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (listToMaybe, mapMaybe)
import GHC.Hs (hsmodImports, ideclAs, ideclName)
import GHC.Types.SrcLoc (unLoc)
import Language.Haskell.Syntax.Module.Name (ModuleName, moduleNameString)

-- | For every local module, the new (always unqualified) name of each of its
-- top-level names.
newtype RenamePlan = RenamePlan
  { rpByModule :: Map ModuleName (Map OccKey String)
  }
  deriving (Show)

-- | The suffix appended to every name of a module: the @as@ alias from the
-- user's own import of it when present, otherwise the last component of
-- the module name (@ToyLib.Parser@ -> @Parser@). Suffix collisions between
-- modules only matter when two same-named definitions meet, and the plan
-- validation catches that.
planSuffixFor :: ParsedFile -> ModuleName -> String
planSuffixFor userFile m =
  maybe lastComponent moduleNameString userAlias
  where
    lastComponent =
      reverse (takeWhile (/= '.') (reverse (moduleNameString m)))
    userAlias =
      listToMaybe . mapMaybe aliasOf . map unLoc . hsmodImports . unLoc $
        pfModule userFile
    aliasOf imp
      | unLoc (ideclName imp) == m = fmap unLoc (ideclAs imp)
      | otherwise = Nothing

-- | Build the plan (default rule, or one @--rename-cmd@ query per name) and
-- validate that the resulting flat namespace has no collisions, including
-- against the user's own top-level names.
mkRenamePlan ::
  Maybe Renamer ->
  ParsedFile ->
  -- | The user file's own symbols (unrenamed, but they occupy names).
  ModuleSymbols ->
  [(LocalModule, ModuleSymbols)] ->
  IO (Either BundleError RenamePlan)
mkRenamePlan mrenamer userFile userSyms locals = runExceptT $ do
  perModule <- traverse planFor locals
  let plan = RenamePlan (Map.fromList perModule)
  ExceptT (pure (validatePlan userSyms perModule))
  pure plan
  where
    planFor (lm, syms) = do
      let suffix = planSuffixFor userFile (lmName lm)
      entries <-
        traverse
          (\(key, kind) -> (,) key <$> newName lm suffix key kind)
          (Map.toAscList (msAll syms))
      pure (lmName lm, Map.fromList entries)

    newName lm suffix (_, old) kind = case mrenamer of
      Nothing -> pure (defaultNewName suffix old)
      Just renamer ->
        ExceptT $
          queryRenamer
            renamer
            RenameQuery
              { rqKind = kindString old kind,
                rqModule = moduleNameString (lmName lm),
                rqSuffix = suffix,
                rqName = old
              }

    -- Operators cannot take an alphanumeric suffix; by default they keep
    -- their name (collisions caught by validation, resolvable via
    -- --rename-cmd).
    defaultNewName suffix old
      | isOperatorString old = old
      | otherwise = old <> suffix

    kindString old kind
      | isOperatorString old = "op"
      | otherwise = case kind of
          SymField -> "field"
          SymDataCon -> "con"
          SymPatSyn -> "con"
          SymTyCon -> "type"
          SymClass -> "type"
          SymValue -> "value"
          SymClassMethod -> "value"

-- | Flat-namespace collision check over every renamed name (per namespace
-- bucket) plus the user file's own top-level names.
validatePlan ::
  ModuleSymbols ->
  [(ModuleName, Map OccKey String)] ->
  Either BundleError ()
validatePlan userSyms perModule =
  case Map.toAscList collisions of
    [] -> Right ()
    ((ns, name), origins) : _ ->
      Left (NameCollision (describe ns name) (sort origins))
  where
    collisions =
      Map.filter (\os -> length os > 1) . Map.fromListWith (<>) $
        [ ((ns, new), [moduleNameString m])
        | (m, entries) <- perModule,
          ((ns, _), new) <- Map.toList entries
        ]
          <> [ ((ns, name), ["<user file>"])
             | (ns, name) <- Map.keys (msAll userSyms)
             ]
    describe ns name = case ns of
      NsValue -> name
      NsData -> name <> " (constructor)"
      NsTcCls -> name <> " (type)"
