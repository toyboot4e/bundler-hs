-- | Tree shaking: which top-level declarations of the bundle are actually
-- reachable from the program's entry points.
--
-- The graph is built before renaming, over the declarations as written, so
-- the names that do not survive never occupy the bundle's flat namespace
-- either (a dropped @Util.sort@ leaves @sort@ free for someone else).
--
-- Reachability is deliberately generous. A declaration's requirements are
-- every 'RdrName' anywhere inside it - local binders included - so the worst
-- an imprecision can do is keep code that was not needed. The one place
-- where something is dropped without being named is a declaration that only
-- ever attaches to another: an instance, a signature, a fixity or an
-- @INLINE@ pragma. Those go when what they attach to goes.
module Bundler.Shake
  ( ModuleFile,
    LiveFile (..),
    LiveSet (..),
    ShakeInput (..),
    keepEverything,
    liveLocal,
    shake,
    userRoots,
  )
where

import Bundler.Parse (ParsedFile (..))
import Bundler.Rename.Apply (ResolveEnv (..))
import Bundler.Symbols
import Data.Generics (Data, listify)
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import GHC.Hs
import GHC.Types.Name.Occurrence (occNameString)
import GHC.Types.Name.Reader (RdrName (..), rdrNameOcc)
import GHC.Types.SrcLoc (GenLocated (..), unLoc)

-- | Which file a top-level name belongs to: a local module, or (Nothing)
-- the user's own file. Matches 'Bundler.Rename.Apply.reSelf'.
type ModuleFile = Maybe ModuleName

-- | A name in the bundle, before renaming: the file that defines it plus
-- its occurrence key.
type GlobalKey = (ModuleFile, OccKey)

-- | What survives in one file.
data LiveFile = LiveFile
  { -- | Indices into the file's top-level declaration list.
    lfDecls :: IntSet,
    -- | The names those declarations define.
    lfKeys :: Set OccKey
  }
  deriving (Show)

data LiveSet = LiveSet
  { lsUser :: LiveFile,
    lsLocals :: Map ModuleName LiveFile
  }
  deriving (Show)

-- | One file as the shaker sees it.
data ShakeInput = ShakeInput
  { siFile :: ModuleFile,
    siDecls :: [LHsDecl GhcPs],
    siSyms :: ModuleSymbols,
    siEnv :: ResolveEnv
  }

-- | What one local module kept; nothing at all for a module the shaker
-- never saw.
liveLocal :: LiveSet -> ModuleName -> LiveFile
liveLocal live m =
  Map.findWithDefault (LiveFile IntSet.empty Set.empty) m (lsLocals live)

-- | The identity result: every declaration of every file survives. Used
-- when tree shaking is off, where the reachability graph is never built at
-- all - hence the plain declaration counts instead of 'ShakeInput's.
keepEverything ::
  (Int, ModuleSymbols) ->
  [(ModuleName, Int, ModuleSymbols)] ->
  LiveSet
keepEverything (userCount, userSyms) locals =
  LiveSet
    { lsUser = wholeFile userCount userSyms,
      lsLocals = Map.fromList [(m, wholeFile n syms) | (m, n, syms) <- locals]
    }

wholeFile :: Int -> ModuleSymbols -> LiveFile
wholeFile n syms =
  LiveFile (IntSet.fromList [0 .. n - 1]) (Map.keysSet (msAll syms))

-- | Reachability over the whole bundle.
--
-- @wholeFiles@ are the files that are not shaken at all (every declaration
-- is a root); @roots@ names what must survive in the files that are.
shake :: Set ModuleFile -> Set GlobalKey -> [ShakeInput] -> LiveSet
shake wholeFiles roots inputs =
  LiveSet
    { lsUser = liveFileOf Nothing,
      lsLocals = Map.fromList [(m, liveFileOf (Just m)) | Just m <- map siFile inputs]
    }
  where
    liveFileOf file =
      LiveFile
        { lfDecls = IntSet.fromList [ndIndex n | n <- kept, ndFile n == file],
          lfKeys =
            Set.fromList
              [key | n <- kept, ndFile n == file, (_, key) <- ndProvides n]
        }

    nodes = concatMap (fileNodes symsOf) inputs
    symsOf = Map.fromList [(m, siSyms si) | si <- inputs, Just m <- [siFile si]]

    providers :: Map GlobalKey [Int]
    providers =
      Map.fromListWith
        (<>)
        [(key, [i]) | (i, n) <- indexed, key <- ndProvides n]

    indexed = zip [0 ..] nodes
    nodeAt = Map.fromList indexed

    -- Only a name some declaration in the bundle defines can gate anything;
    -- everything else in a trigger is external and never blocks.
    localTrigger keys = filter (`Map.member` providers) keys

    initial =
      IntSet.fromList
        ( [i | (i, n) <- indexed, ndFile n `Set.member` wholeFiles]
            <> [i | (i, n) <- indexed, ndRule n == Always]
            <> concat [Map.findWithDefault [] key providers | key <- Set.toList roots]
        )

    kept = map (nodeAt Map.!) (IntSet.toList (fixpoint initial Set.empty initial))

    conditionals = [(i, n) | (i, n) <- indexed, isConditional (ndRule n)]
    isConditional r = case r of
      AllOf _ -> True
      AnyOf _ -> True
      _ -> False

    -- Worklist: each declaration is expanded once, when it first goes live.
    -- The declarations that only attach to something else are re-checked
    -- every round instead, because a name going live can pull one in.
    fixpoint live liveKeys frontier
      | IntSet.null frontier = live
      | otherwise = fixpoint (live <> added) liveKeys' added
      where
        newNodes = map (nodeAt Map.!) (IntSet.toList frontier)
        liveKeys' = liveKeys <> Set.fromList (concatMap ndProvides newNodes)
        needed =
          concat
            [ Map.findWithDefault [] key providers
            | key <- concatMap ndRequires newNodes
            ]
        attached = [i | (i, n) <- conditionals, satisfied liveKeys' (ndRule n)]
        added = IntSet.fromList (needed <> attached) `IntSet.difference` live

    satisfied liveKeys rule = case rule of
      AllOf keys -> all (`Set.member` liveKeys) (localTrigger keys)
      AnyOf keys -> case localTrigger keys of
        -- Nothing local to attach to, so nothing can take it away either.
        [] -> True
        ks -> any (`Set.member` liveKeys) ks
      _ -> False

-- | When a declaration is kept.
data KeepRule
  = -- | Kept when something else needs one of the names it defines.
    ByName
  | -- | Never named by anything, always kept (@default (Int)@ and friends).
    Always
  | -- | An instance: kept while every local name of its head is kept.
    AllOf [GlobalKey]
  | -- | A signature or pragma: kept while any name it annotates is kept.
    AnyOf [GlobalKey]
  deriving (Eq)

data Node = Node
  { ndFile :: ModuleFile,
    ndIndex :: Int,
    ndProvides :: [GlobalKey],
    ndRequires :: [GlobalKey],
    ndRule :: KeepRule
  }

fileNodes :: Map ModuleName ModuleSymbols -> ShakeInput -> [Node]
fileNodes symsOf si =
  [ Node
      { ndFile = siFile si,
        ndIndex = i,
        ndProvides = [(siFile si, key) | (key, _, _, _) <- declBinders decl],
        ndRequires = resolveAll (namesIn decl),
        ndRule = ruleFor decl
      }
  | (i, L _ decl) <- zip [0 ..] (siDecls si)
  ]
  where
    resolveAll = concatMap (resolveRdr symsOf (siSyms si) (siEnv si))

    ruleFor decl = case decl of
      ValD {} -> ByName
      TyClD {} -> ByName
      ForD {} -> ByName
      SigD _ sig -> AnyOf (resolveAll (sigSubjects sig))
      KindSigD _ (StandaloneKindSig _ n _) -> AnyOf (resolveAll [unLoc n])
      RoleAnnotD _ (RoleAnnotDecl _ n _) -> AnyOf (resolveAll [unLoc n])
      InstD _ inst -> AllOf (resolveAll (instHeadNames inst))
      DerivD _ (DerivDecl {deriv_type = ty}) ->
        AllOf (resolveAll (typeHeadNames (hswc_body ty)))
      -- Rules, annotations and deprecations name things without defining
      -- them; keeping one resurrects what it names, which is safe.
      RuleD {} -> AnyOf (resolveAll (namesIn decl))
      AnnD {} -> AnyOf (resolveAll (namesIn decl))
      WarningD {} -> AnyOf (resolveAll (namesIn decl))
      _ -> Always

-- | Every 'RdrName' written inside a declaration. Local binders are in
-- there too: over-approximating what a declaration needs can only keep
-- code alive, never drop something that is used.
namesIn :: (Data a) => a -> [RdrName]
namesIn = listify (const True)

-- | The names a signature or pragma annotates - not the types it mentions.
-- An unrecognized one falls back to everything it names, which keeps it
-- attached to something rather than dropping it silently.
sigSubjects :: Sig GhcPs -> [RdrName]
sigSubjects sig = case sig of
  TypeSig _ ns _ -> map unLoc ns
  PatSynSig _ ns _ -> map unLoc ns
  FixSig _ (FixitySig _ ns _) -> map unLoc ns
  InlineSig _ n _ -> [unLoc n]
  SCCFunSig _ n _ -> [unLoc n]
  _ -> namesIn sig

-- | The class and type constructors of an instance head, which is what
-- decides whether the instance still has anything to attach to. The
-- context is deliberately left out: it is a requirement of the instance,
-- not a reason for it to exist.
instHeadNames :: InstDecl GhcPs -> [RdrName]
instHeadNames inst = case inst of
  ClsInstD _ (ClsInstDecl {cid_poly_ty = ty}) -> typeHeadNames ty
  DataFamInstD _ (DataFamInstDecl {dfid_eqn = eqn}) -> famEqnNames eqn
  TyFamInstD _ (TyFamInstDecl {tfid_eqn = eqn}) -> famEqnNames eqn

famEqnNames :: FamEqn GhcPs r -> [RdrName]
famEqnNames FamEqn {feqn_tycon = n, feqn_pats = pats} = unLoc n : namesIn pats

typeHeadNames :: LHsSigType GhcPs -> [RdrName]
typeHeadNames sigTy = namesIn (strip (sig_body (unLoc sigTy)))
  where
    strip ty = case unLoc ty of
      HsForAllTy {hst_body = body} -> strip body
      HsQualTy {hst_body = body} -> strip body
      HsParTy _ inner -> strip inner
      _ -> ty

-- | Where a written name can be defined, as far as the file's imports say.
-- Several answers mean the reference is ambiguous to the bundler; all of
-- them are followed, so nothing is dropped on a guess.
resolveRdr ::
  Map ModuleName ModuleSymbols ->
  ModuleSymbols ->
  ResolveEnv ->
  RdrName ->
  [GlobalKey]
resolveRdr symsOf ownSyms env rdr = case rdr of
  Unqual _ -> own <> imported
  Qual q _ -> case Map.lookup q (reQualLocal env) of
    Just m -> [(Just m', key) | m' <- definer m]
    Nothing -> []
  _ -> []
  where
    key = (nsKeyOf occ, occNameString occ)
    occ = rdrNameOcc rdr
    own = [(reSelf env, key) | key `Map.member` msAll ownSyms]
    imported =
      [ (Just m, key)
      | (m, _) <- fromMaybe [] (Map.lookup key (reUnqualLocal env))
      ]
    -- A name written through a local module may be defined there or
    -- re-exported from another local module.
    definer m
      | maybe False (Map.member key . msAll) (Map.lookup m symsOf) = [m]
      | otherwise =
          maybe [] pure (Map.lookup m symsOf >>= Map.lookup key . msReExported)

-- | The names that must survive in the user's own file, or 'Nothing' when
-- it cannot be shaken at all.
--
-- An export list says exactly what the outside world needs. Without one,
-- a @Main@ module (or a file with no module header, which is @Main@) is
-- entered through @main@ alone. Any other module exports everything it
-- defines, so there is nothing to shake.
userRoots :: ParsedFile -> ModuleSymbols -> Maybe (Set OccKey)
userRoots pf syms
  | Just _ <- hsmodExports modl = Just (msExported syms)
  | isMain, mainKey `Map.member` msAll syms = Just (Set.singleton mainKey)
  | otherwise = Nothing
  where
    modl = unLoc (pfModule pf)
    isMain = maybe True ((== "Main") . moduleNameString . unLoc) (hsmodName modl)
    mainKey = (NsValue, "main")
