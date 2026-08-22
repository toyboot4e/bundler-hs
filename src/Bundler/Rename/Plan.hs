module Bundler.Rename.Plan
  ( RenamePlan (..),
    mkRenamePlan,
  )
where

import Bundler.Discovery
import Bundler.Error
import Bundler.Parse
import Bundler.PreludeNames (preludeNames)
import Bundler.RenameCmd
import Bundler.Symbols
import Control.Monad.Trans.Except (ExceptT (..), runExceptT)
import Data.Char (isUpper)
import Data.Containers.ListUtils (nubOrd)
import Data.List (sort, stripPrefix)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import GHC.Hs
  ( IE (..),
    ImportDecl (..),
    ImportDeclQualifiedStyle (..),
    ImportListInterpretation (..),
    hsmodImports,
    ideclAs,
    ideclName,
    ieWrappedName,
  )
import GHC.Types.SrcLoc (GenLocated (..), unLoc)
import Language.Haskell.Syntax.Module.Name (ModuleName, moduleNameString)

-- | For every local module, the new (always unqualified) name of each of its
-- top-level names.
newtype RenamePlan = RenamePlan
  { rpByModule :: Map ModuleName (Map OccKey String)
  }
  deriving (Show)

-- | Build the plan (default rule, or one @--rename-cmd@ query per name) and
-- validate that the resulting flat namespace has no collisions, including
-- against the user's own top-level names.
--
-- Only the names in each 'ModuleSymbols' are planned, so a caller that has
-- shaken dead declarations out passes symbol tables restricted to what
-- survives and those names stop occupying the flat namespace.
mkRenamePlan ::
  Maybe Renamer ->
  ParsedFile ->
  -- | The user file's own symbols (unrenamed, but they occupy names).
  ModuleSymbols ->
  -- | Names the bundle takes from external imports
  -- ('Bundler.Shake.wnExternal').
  Set OccKey ->
  -- | Local modules the bundle writes a qualified name into
  -- ('Bundler.Shake.wnQualified').
  Set ModuleName ->
  [(LocalModule, ModuleSymbols)] ->
  IO (Either BundleError RenamePlan)
mkRenamePlan mrenamer userFile userSyms external qualifiedUses locals = runExceptT $ do
  perModule <- traverse planFor (defaultNames userFile userSyms external qualifiedUses locals)
  let plan = RenamePlan (Map.fromList perModule)
  ExceptT (pure (validatePlan userSyms perModule))
  pure plan
  where
    planFor (lm, syms, defaults) = do
      entries <-
        traverse
          (\(key, def) -> (,) key <$> newName lm syms key def)
          (Map.toAscList defaults)
      pure (lmName lm, Map.fromList entries)

    -- The rename command sees the suffix the default rule settled on, so
    -- `echo "$name$suffix"` still reproduces the default behavior - including
    -- the empty suffix of a name that keeps its original spelling.
    newName lm syms key@(_, old) def = case mrenamer of
      Nothing -> pure def
      Just renamer ->
        ExceptT $
          queryRenamer
            renamer
            RenameQuery
              { rqKind = kindString old (Map.lookup key (msAll syms)),
                rqModule = moduleNameString (lmName lm),
                rqName = old,
                rqSuffix = fromMaybe "" (stripPrefix old def)
              }

    kindString old kind
      | isOperatorString old = "op"
      | otherwise = case kind of
          Just SymField -> "field"
          Just SymDataCon -> "con"
          Just SymPatSyn -> "con"
          Just SymTyCon -> "type"
          Just SymClass -> "type"
          _ -> "value"

-- | The name every top-level name of every local module gets before
-- @--rename-cmd@ has its say.
--
-- A name keeps its original spelling when nothing else claims it (see
-- 'keptNames'); the rest take their module's suffix, the shortest one from
-- 'suffixCandidates' that keeps the flat namespace collision-free. Modules
-- are decided in dependency order, and each one claims the names it takes.
defaultNames ::
  ParsedFile ->
  ModuleSymbols ->
  Set OccKey ->
  Set ModuleName ->
  [(LocalModule, ModuleSymbols)] ->
  [(LocalModule, ModuleSymbols, Map OccKey String)]
defaultNames userFile userSyms external qualifiedUses locals = go claimed0 locals
  where
    go _ [] = []
    go claimed ((lm, syms) : rest) =
      (lm, syms, names) : go (claimed <> claims names) rest
      where
        keysOf p = filter p (Map.keys (msAll syms))
        -- Operators cannot take an alphanumeric suffix, so they always keep
        -- their name (a collision is caught by validation and resolvable
        -- via --rename-cmd).
        asIs = keysOf (\key -> isOperatorString (snd key) || (lmName lm, key) `Set.member` kept)
        suffixed = keysOf (\key -> not (isOperatorString (snd key)) && (lmName lm, key) `Set.notMember` kept)
        suffix = pick (suffixCandidates userFile (lmName lm))
        pick [] = ""
        pick [c] = c
        pick (c : cs)
          | all (free . withSuffix c) suffixed = c
          | otherwise = pick cs
        free key = key `Set.notMember` claimed && key `Set.notMember` occupied
        withSuffix s (ns, old) = (ns, old <> s)
        names =
          Map.fromList $
            [(key, snd key) | key <- asIs]
              <> [(key, snd (withSuffix suffix key)) | key <- suffixed]

    claims names = Set.fromList [(ns, new) | ((ns, _), new) <- Map.toList names]

    -- Names nothing in the bundle can move out of the way: the user's own
    -- top level, what the implicit Prelude provides, the names the user's
    -- own unqualified imports of external modules bring in, and every name
    -- the bundle writes that only an external import can be providing.
    occupied =
      Map.keysSet (msAll userSyms)
        <> preludeNames
        <> external
        <> externalImportNames localNames userFile

    -- Operators keep their names wherever they are defined, and kept names
    -- belong to their module from the start, so both are claimed before any
    -- module picks a suffix.
    claimed0 =
      Set.fromList
        ( [key | (_, syms) <- locals, key <- Map.keys (msAll syms), isOperatorString (snd key)]
            <> [key | (_, key) <- Set.toList kept]
        )

    kept = keptNames occupied qualifiedModules locals
    localNames = Set.fromList (map (lmName . fst) locals)
    -- Naming a module, by importing it qualified or by writing one of its
    -- names as @M.f@, is a choice the bundle keeps: those names are always
    -- renamed. A qualified write counts even when it goes through a
    -- re-export module, because it lands on the module that defines it.
    qualifiedModules =
      qualifiedUses
        <> Set.fromList
          [ unLoc (ideclName imp)
          | pf <- userFile : map (lmParsed . fst) locals,
            imp <- map unLoc (hsmodImports (unLoc (pfModule pf))),
            ideclQualified imp /= NotQualified
          ]

-- | The names that keep their original spelling in the bundle.
--
-- A module qualifies when nothing in the bundle names it: no @qualified@
-- import of it and no @M.f@ written into it. Its names are then only ever
-- written the way they were defined, so a suffix buys nothing. Within those modules a name survives unrenamed when exactly one
-- of them defines it and it is not already taken (the user's own names,
-- Prelude, the user's import lists, or anything the bundle takes from an
-- external import).
keptNames ::
  Set OccKey ->
  Set ModuleName ->
  [(LocalModule, ModuleSymbols)] ->
  Set (ModuleName, OccKey)
keptNames occupied qualifiedModules locals =
  Set.fromList
    [ (m, key)
    | (key, [m]) <- Map.toList candidates,
      key `Set.notMember` occupied
    ]
  where
    candidates =
      Map.fromListWith
        (<>)
        [ (key, [lmName lm])
        | (lm, syms) <- locals,
          lmName lm `Set.notMember` qualifiedModules,
          key <- Map.keys (msAll syms),
          not (isOperatorString (snd key))
        ]

-- | The suffixes to try for one module, shortest and most speaking first:
-- the alias of the user's own @qualified ... as@ import, the initials of the
-- module's last component (@SuffixArray@ -> @SA@), that component itself,
-- and finally the whole dotted name flattened (@Data.Deque@ -> @DataDeque@).
suffixCandidates :: ParsedFile -> ModuleName -> [String]
suffixCandidates userFile m =
  nubOrd (filter (not . null) (userAlias <> [initials, lastComponent, flattened]))
  where
    components = splitOn '.' (moduleNameString m)
    lastComponent = last components
    flattened = concat components
    initials = filter isUpper lastComponent
    userAlias =
      take 1 . mapMaybe aliasOf . map unLoc . hsmodImports . unLoc $
        pfModule userFile
    aliasOf imp
      | unLoc (ideclName imp) == m = fmap (moduleNameString . unLoc) (ideclAs imp)
      | otherwise = Nothing
    splitOn c s = case break (== c) s of
      (chunk, _ : rest) -> chunk : splitOn c rest
      (chunk, []) -> [chunk]

-- | The names the user's own unqualified imports of external modules put in
-- unqualified scope, as far as they can be known without package interfaces:
-- an explicit list names them, a @hiding@ or open import does not (those are
-- the blind spot documented in the README).
externalImportNames :: Set ModuleName -> ParsedFile -> Set OccKey
externalImportNames localNames userFile =
  Set.fromList
    [ key
    | imp <- map unLoc (hsmodImports (unLoc (pfModule userFile))),
      unLoc (ideclName imp) `Set.notMember` localNames,
      NotQualified <- [ideclQualified imp],
      Just (Exactly, L _ items) <- [ideclImportList imp],
      key <- concatMap (itemKeys . unLoc) items
    ]
  where
    wrappedKey = occKeyOf . ieWrappedName . unLoc
    -- The children of T(..) are unknowable; T itself is enough to keep a
    -- kept name from stepping on the type.
    itemKeys ie = case ie of
      IEVar _ n _ -> [wrappedKey n]
      IEThingAbs _ n _ -> [wrappedKey n]
      IEThingAll _ n _ -> [wrappedKey n]
      IEThingWith _ n _ subs _ ->
        wrappedKey n : concatMap (bothNamespaces . wrappedKey) subs
      _ -> []
    bothNamespaces (_, name) = [(NsValue, name), (NsData, name)]

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
