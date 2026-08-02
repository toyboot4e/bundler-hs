-- The GHC AST's trees-that-grow extension constructors make record updates
-- formally incomplete everywhere; they are unreachable at GhcPs.
{-# OPTIONS_GHC -Wno-incomplete-record-updates #-}

module Bundler.Rename.Apply
  ( ResolveEnv (..),
    mkResolveEnv,
    applyRenames,
    applyRenamesPatched,
  )
where

import Bundler.Error
import Bundler.Parse
import Bundler.Rename.Plan
import Bundler.Render (renderSDoc)
import Bundler.SourcePatch (Patch)
import Bundler.Symbols
import Control.Monad (when)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Writer.CPS (WriterT, censor, runWriterT, tell)
import Data.Containers.ListUtils (nubOrd)
import Data.Generics (Data, everywhereM, extM, gmapM, mkM)
import Data.List (intercalate)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import GHC.Hs
import GHC.Types.Name.Occurrence
  ( mkOccName,
    mkVarOcc,
    occNameSpace,
    occNameString,
  )
import GHC.Types.Name.Reader (RdrName (..), mkRdrUnqual, rdrNameOcc)
import GHC.Types.SrcLoc (GenLocated (..), SrcSpan (..), unLoc)
import GHC.Utils.Outputable (ppr)

type M = Either BundleError

-- | The rename traversal's monad: alongside the rewritten AST it records
-- span-anchored text patches, so a file can also be re-emitted as its
-- original source text with just the renames spliced in (preserving
-- comments and formatting - used for the user's own file).
type RM = WriterT [Patch] M

liftE :: M a -> RM a
liftE = lift

-- | Record a replacement for a real span.
patchAt :: SrcSpan -> String -> RM ()
patchAt (RealSrcSpan real _) new = tell [(real, new)]
patchAt _ _ = pure ()

-- | How names written in one particular file resolve to local modules.
data ResolveEnv = ResolveEnv
  { -- | The module the rewritten file itself defines ('Nothing' for the
    -- user's file, whose own names are never renamed).
    reSelf :: Maybe ModuleName,
    -- | Written qualifier (alias or module name) -> local module.
    reQualLocal :: Map ModuleName ModuleName,
    -- | Names reachable unqualified via imports of local modules -> their
    -- planned new names (several entries = ambiguous if actually used).
    reUnqualLocal :: Map OccKey [(ModuleName, String)],
    -- | Written qualifier -> external module; references are rewritten to
    -- the canonical (fully-qualified) form. Only populated for library
    -- files: the user's own imports survive verbatim.
    reQualExt :: Map ModuleName ModuleName,
    -- | Names a library file imported from an external module via an
    -- explicit import list; the import is dropped and uses are rewritten to
    -- the canonical qualified form.
    reUnqualExt :: Map OccKey ModuleName,
    -- | A library file's open / @hiding@ / complex-list unqualified external
    -- imports: origins of individual names are unknowable without package
    -- interfaces, so these are kept verbatim (GHC's scope-union semantics
    -- make repeated imports of one module behave correctly).
    reOpenExtImports :: [LImportDecl GhcPs],
    -- | Canonical qualifier per external module (default: the module name
    -- itself; overridable via @--rename-cmd@'s @extmod@ kind). Injected
    -- after environment construction, once the set of externals is known.
    reExtAlias :: Map ModuleName ModuleName
  }

-- | Build the environment for one file from its imports of local modules.
-- A non-qualified import also allows qualified access under the same name,
-- so every local import contributes its qualifier; non-qualified imports
-- additionally bring (a subset of) the module's exports into unqualified
-- scope, honoring explicit and @hiding@ lists.
mkResolveEnv ::
  RenamePlan ->
  Map ModuleName ModuleSymbols ->
  Maybe ModuleName ->
  ParsedFile ->
  Either BundleError ResolveEnv
mkResolveEnv plan symsOf self pf = do
  case qualConflicts of
    (q, ms) : _ ->
      Left (QualifierConflict (moduleNameString q) (map moduleNameString ms))
    [] -> Right ()
  unqual <- mconcat <$> traverse (unqualsOf . unLoc) imports
  let unqualLocal = Map.unionsWith (<>) unqual
  pure
    ResolveEnv
      { reSelf = self,
        reQualLocal =
          Map.fromList
            [ (maybe m unLoc (ideclAs imp), m)
            | imp <- map unLoc imports,
              let m = unLoc (ideclName imp),
              m `Map.member` symsOf
            ],
        reUnqualLocal = unqualLocal,
        reQualExt = if isLibrary then Map.fromList (concatMap (qualExtOf . unLoc) imports) else Map.empty,
        reUnqualExt = if isLibrary then Map.fromList (concatMap (unqualExtOf . unLoc) imports) else Map.empty,
        reOpenExtImports =
          if isLibrary
            then mapMaybe (prunePrelude unqualLocal) (filter (isOpenExt . unLoc) imports)
            else [],
        reExtAlias = Map.empty
      }
  where
    imports = hsmodImports (unLoc (pfModule pf))
    isLibrary = self /= Nothing
    wrappedKey = occKeyOf . ieWrappedName . unLoc

    isExternal imp = not (unLoc (ideclName imp) `Map.member` symsOf)

    -- One qualifier naming two different modules cannot be attributed
    -- reliably (the bundler does not scope-union like GHC), so it is
    -- rejected wherever references are rewritten: everywhere in a library
    -- file, and for qualifiers involving a local module in the user's file
    -- (whose external imports survive verbatim).
    qualConflicts =
      [ (q, ms)
      | (q, mset) <- Map.toAscList qualTargets,
        let ms = Set.toAscList mset,
        length ms > 1,
        isLibrary || any (`Map.member` symsOf) ms
      ]
    qualTargets =
      Map.fromListWith
        Set.union
        [ (maybe m unLoc (ideclAs imp), Set.singleton m)
        | imp <- map unLoc imports,
          let m = unLoc (ideclName imp)
        ]

    -- Every external import (any style) allows qualified access via its
    -- alias or module name; all such references become canonical.
    qualExtOf imp
      | isExternal imp =
          [(maybe m unLoc (ideclAs imp), m)]
      | otherwise = []
      where
        m = unLoc (ideclName imp)

    -- Explicit-list unqualified externals: origins are exact, so the
    -- import is dropped and each listed name rewrites to qualified form.
    unqualExtOf imp
      | isExternal imp,
        NotQualified <- ideclQualified imp,
        Just (Exactly, L _ items) <- ideclImportList imp,
        Just keys <- traverse (simpleItemKeys . unLoc) items =
          [(key, unLoc (ideclName imp)) | key <- concat keys]
      | otherwise = []

    -- Import-list items whose names are fully known without package
    -- interfaces. IEThingAll (@T(..)@) is not: its children are unknown.
    simpleItemKeys :: IE GhcPs -> Maybe [OccKey]
    simpleItemKeys ie = case ie of
      IEVar _ n _ -> Just [wrappedKey n]
      IEThingAbs _ n _ -> Just [wrappedKey n]
      IEThingWith _ n _ subs _ ->
        Just (wrappedKey n : concatMap (bothKeys . wrappedKey) subs)
      _ -> Nothing
      where
        bothKeys (_, name) = [(NsValue, name), (NsData, name)]

    isOpenExt imp =
      isExternal imp
        && ideclQualified imp == NotQualified
        && null (unqualExtOf imp)

    -- In the merged module any explicit Prelude import cancels the implicit
    -- one for the entire bundle, so a library's kept-verbatim
    -- @import Prelude hiding (...)@ would hide those names from the user's
    -- code too. A hidden name whose clashing definition is renamed anyway -
    -- defined by this module, or reached through a local import whose uses
    -- are rewritten - no longer needs hiding and leaves the list; when the
    -- list empties the import is dropped. An explicit list brings in a
    -- subset of what the implicit Prelude provides, so it is dropped
    -- outright. Hiding lists of other modules only restrict that module's
    -- own exports and are kept as they are.
    prunePrelude ::
      Map OccKey [(ModuleName, String)] ->
      LImportDecl GhcPs ->
      Maybe (LImportDecl GhcPs)
    prunePrelude unqualLocal limp@(L l imp)
      | unLoc (ideclName imp) /= mkModuleName "Prelude" = Just limp
      | Just (EverythingBut, L ll items) <- ideclImportList imp =
          case filter (not . obsoleteHiding unqualLocal . unLoc) items of
            [] -> Nothing
            kept -> Just (L l imp {ideclImportList = Just (EverythingBut, L ll kept)})
      | otherwise = Nothing

    -- Is hiding this item obsolete once the bundle's renames are applied?
    -- A @T(..)@ item also hides Prelude's children of T, which are
    -- unknowable; the module's own children of T are the closest stand-in.
    obsoleteHiding unqualLocal ie = case ie of
      IEVar _ n _ -> covered (wrappedKey n)
      IEThingAbs _ n _ -> covered (wrappedKey n)
      IEThingAll _ n _ ->
        let key@(_, name) = wrappedKey n
         in renamedAway key && all renamedAway (selfChildren name)
      IEThingWith _ n _ subs _ ->
        renamedAway (wrappedKey n)
          && all
            (\(_, s) -> any renamedAway [(NsValue, s), (NsData, s)])
            (map wrappedKey subs)
      _ -> False
      where
        covered key = renamedAway key || localUse key
        -- Renamed to a different name: the definition no longer occupies
        -- the original one (operators keep theirs and stay hidden).
        renamedAway key@(_, name) = case Map.lookup key renamedHere of
          Just new -> new /= name
          Nothing -> False
        -- Unambiguously provided by a local import: uses are rewritten to
        -- the planned name.
        localUse key = case Map.lookup key unqualLocal of
          Just [_] -> True
          _ -> False

    renamedHere = fromMaybe Map.empty (self >>= (`Map.lookup` rpByModule plan))

    selfChildren name =
      fromMaybe [] (self >>= (`Map.lookup` symsOf) >>= Map.lookup name . msChildren)

    -- Names one import brings into unqualified scope, attributed to the
    -- module that defines them (following re-exports) and mapped to their
    -- planned new names.
    unqualsOf :: ImportDecl GhcPs -> M [Map OccKey [(ModuleName, String)]]
    unqualsOf imp
      | NotQualified <- ideclQualified imp,
        Just syms <- Map.lookup m symsOf = do
          origins <-
            either
              (\name -> Left (NotExported name (moduleNameString m)))
              Right
              (importVisibleOrigins (`Map.lookup` symsOf) m syms imp)
          pure
            [ Map.fromList
                [ (key, [(origin, new)])
                | (key, origin) <- Map.toList origins,
                  Just new <- [Map.lookup origin (rpByModule plan) >>= Map.lookup key]
                ]
            ]
      | otherwise = pure []
      where
        m = unLoc (ideclName imp)

-- | Names currently shadowed by enclosing lambda/let/where/case/do/
-- comprehension binders.
type Shadow = Set OccKey

-- | Rewrite every 'RdrName' occurrence in the declarations according to the
-- plan, discarding the collected patches.
applyRenames ::
  RenamePlan ->
  Map ModuleName ModuleSymbols ->
  ResolveEnv ->
  [LHsDecl GhcPs] ->
  Either BundleError [LHsDecl GhcPs]
applyRenames plan symsOf env decls =
  fst <$> applyRenamesPatched plan symsOf env decls

-- | Rewrite every 'RdrName' occurrence in the declarations according to the
-- plan, tracking local scope so shadowed names stay untouched. Also
-- returns one text patch per changed occurrence, anchored to the original
-- source span, so the caller can alternatively re-emit the original text
-- with just the renames spliced in.
--
-- The traversal is generic ('gmapM') with type-specific cases for every
-- construct that introduces binders. Binders scope over sibling subtrees
-- (e.g. pattern binders over the equation body), which is why this cannot
-- be a plain @everywhereM@.
applyRenamesPatched ::
  RenamePlan ->
  Map ModuleName ModuleSymbols ->
  ResolveEnv ->
  [LHsDecl GhcPs] ->
  Either BundleError ([LHsDecl GhcPs], [Patch])
applyRenamesPatched plan symsOf env decls = runWriterT $ do
  expanded <- traverse (expandWildcards plan symsOf env) decls
  traverse (go Set.empty) expanded
  where
    go :: (Data a) => Shadow -> a -> RM a
    go sc =
      gen
        `extM` (locRdrCase sc)
        `extM` (rdrCase sc)
        `extM` (exprCase sc)
        `extM` (matchCase sc)
        `extM` (grhsCase sc)
        `extM` (bindCase sc)
        `extM` (instCase sc)
      where
        gen :: (Data d) => d -> RM d
        gen = gmapM (go sc)

    binders :: (CollectFlag GhcPs -> a -> [IdP GhcPs]) -> a -> Shadow
    binders collect x =
      Set.fromList (map occKeyOf (collect CollNoDictBinders x))

    -- Occurrences carry their span at the 'LocatedN' level: rewrite there
    -- and record the patch when the name actually changed. The span
    -- includes any adornment (backticks, parens), so the replacement must
    -- carry it too.
    locRdrCase :: Shadow -> LocatedN RdrName -> RM (LocatedN RdrName)
    locRdrCase sc lrdr@(L l rdr) = do
      rdr' <- rdrCase sc rdr
      when (rdr' /= rdr) $
        patchAt (getLocA lrdr) (adorn (renderSDoc (ppr rdr')))
      pure (L l rdr')
      where
        adorn text = case anns l of
          NameAnn {nann_adornment = NameParens {}} -> "(" <> text <> ")"
          NameAnn {nann_adornment = NameParensHash {}} -> "(# " <> text <> " #)"
          NameAnn {nann_adornment = NameBackquotes {}} -> "`" <> text <> "`"
          NameAnn {nann_adornment = NameSquare {}} -> "[" <> text <> "]"
          _ -> text

    -- Leaf rewrite. Qualified references cannot be shadowed; unqualified
    -- ones consult the shadow set first, then the module's own top level,
    -- then unqualified imports of local modules.
    rdrCase :: Shadow -> RdrName -> RM RdrName
    rdrCase sc rdr = case rdr of
      Unqual occ
        | occKeyOf rdr `Set.member` sc -> pure rdr
        | Just self <- reSelf env,
          Just new <- lookupPlan self (occKeyOf rdr) ->
            pure (unqual occ new)
        | otherwise -> case Map.lookup (occKeyOf rdr) (reUnqualLocal env) of
            Nothing -> case Map.lookup (occKeyOf rdr) (reUnqualExt env) of
              Just m -> pure (Qual (extAliasOf m) occ)
              Nothing -> pure rdr
            -- One name may arrive via several imports (e.g. directly and
            -- through a re-export module); same origin means no ambiguity.
            Just entries -> case nubOrd entries of
              [(_, new)] -> pure (unqual occ new)
              several ->
                liftE . Left $
                  AmbiguousName
                    (occNameString occ)
                    (map (moduleNameString . fst) several)
      Qual q occ
        | Just m <- Map.lookup q (reQualLocal env) ->
            case plannedFor m (occKeyOf (Unqual occ)) of
              Just new -> pure (unqual occ new)
              Nothing ->
                liftE . Left $
                  UnknownQualifiedName
                    (moduleNameString q <> "." <> occNameString occ)
                    (moduleNameString m)
        | Just m <- Map.lookup q (reQualExt env) ->
            pure (Qual (extAliasOf m) occ)
      _ -> pure rdr

    extAliasOf m = Map.findWithDefault m m (reExtAlias env)

    lookupPlan m key = Map.lookup m (rpByModule plan) >>= Map.lookup key

    -- A name qualified by a local module may be one of its own binders or
    -- something it re-exports from another local module.
    plannedFor m key = case lookupPlan m key of
      Just new -> Just new
      Nothing -> do
        syms <- Map.lookup m symsOf
        origin <- Map.lookup key (msReExported syms)
        lookupPlan origin key

    unqual occ new = mkRdrUnqual (mkOccName (occNameSpace occ) new)

    -- One equation/alternative: pattern binders scope over the patterns
    -- themselves (view patterns, and protecting the binder occurrences),
    -- the guards, the RHS, and the where block.
    matchCase :: Shadow -> Match GhcPs (LHsExpr GhcPs) -> RM (Match GhcPs (LHsExpr GhcPs))
    matchCase sc m@(Match {m_ctxt = ctxt, m_pats = pats, m_grhss = grhss}) = do
      let sc' = sc <> binders collectPatsBinders (unLoc pats)
          scAll = sc' <> binders collectLocalBinders (grhssLocalBinds grhss)
      -- The equation's own name lives in the context (the printer renders
      -- it from here, not from fun_id) and is resolved in the *outer*
      -- scope: the function's arguments never shadow its printed name.
      ctxt' <- go sc ctxt
      pats' <- go sc' pats
      grhss' <- go scAll grhss
      pure m {m_ctxt = ctxt', m_pats = pats', m_grhss = grhss'}

    -- One guarded RHS: pattern guards bind left-to-right into later guards
    -- and the body.
    grhsCase :: Shadow -> GRHS GhcPs (LHsExpr GhcPs) -> RM (GRHS GhcPs (LHsExpr GhcPs))
    grhsCase sc (GRHS x guards body) = do
      (sc', guards') <- threadStmts sc guards
      body' <- go sc' body
      pure (GRHS x guards' body')

    -- A pattern binding's where block scopes over guards and RHS. (The
    -- pattern's own binders belong to the enclosing scope and are handled
    -- by whoever collected them - top level or let/where.)
    bindCase :: Shadow -> HsBind GhcPs -> RM (HsBind GhcPs)
    bindCase sc b = case b of
      PatBind {pat_lhs = lhs, pat_rhs = grhss} -> do
        let scAll = sc <> binders collectLocalBinders (grhssLocalBinds grhss)
        lhs' <- go sc lhs
        grhss' <- go scAll grhss
        pure b {pat_lhs = lhs', pat_rhs = grhss'}
      _ -> gmapM (go sc) b

    exprCase :: Shadow -> HsExpr GhcPs -> RM (HsExpr GhcPs)
    exprCase sc e = case e of
      -- let/where bindings are recursive: binders scope over their own
      -- right-hand sides as well as the body.
      HsLet x binds body -> do
        let sc' = sc <> binders collectLocalBinders binds
        binds' <- go sc' binds
        body' <- go sc' body
        pure (HsLet x binds' body')
      -- do blocks and (monad/list) comprehensions: each statement's
      -- binders scope over the statements after it.
      HsDo x ctx (L l stmts) -> do
        (_, stmts') <- threadStmts sc stmts
        pure (HsDo x ctx (L l stmts'))
      _ -> gmapM (go sc) e

    -- Instance declarations: method binders resolve via the class of the
    -- instance head, not lexically. A local class's methods are renamed
    -- with the class's own module plan; an external class's methods are
    -- left alone even when a same-named local export is in scope. The
    -- bodies still get the normal lexical treatment; only the binder
    -- names are overridden afterwards - so the lexical pass's patches for
    -- those spans are censored and re-recorded with the overridden names.
    instCase :: Shadow -> ClsInstDecl GhcPs -> RM (ClsInstDecl GhcPs)
    instCase sc inst = do
      let binderOccs :: [(SrcSpan, String)]
          binderOccs =
            concat
              [ (getLocA fid, nameOf (unLoc fid))
                  : [ (getLocA f, nameOf (unLoc f))
                    | L _ m <- unLoc (mg_alts mg),
                      FunRhs {mc_fun = f} <- [m_ctxt m]
                    ]
              | L _ FunBind {fun_id = fid, fun_matches = mg} <- cid_binds inst
              ]
              <> [ (getLocA n, nameOf (unLoc n))
                 | L _ (TypeSig _ ns _) <- cid_sigs inst,
                   n <- ns
                 ]
          binderSpans =
            Set.fromList [real | (RealSrcSpan real _, _) <- binderOccs]
      inst' <-
        censor
          (filter (\(sp, _) -> sp `Set.notMember` binderSpans))
          (gmapM (go sc) inst)
      let clsPlan = do
            cls <- headClassName (cid_poly_ty inst)
            m <- resolveToModule (unLoc cls)
            Map.lookup m (rpByModule plan)
          methodName old = case clsPlan of
            Nothing -> old
            Just p -> fromMaybe old (Map.lookup ((NsValue, old)) p)
          fixOne orig (L l b) = L l (setMethodName (methodName orig) b)
      sequence_
        [ patchAt sp new
        | (sp, old) <- binderOccs,
          let new = methodName old,
          new /= old
        ]
      pure
        inst'
          { cid_binds =
              zipWith
                fixOne
                (map bindName (cid_binds inst))
                (cid_binds inst'),
            cid_sigs =
              zipWith
                (fixSigNames methodName)
                (cid_sigs inst)
                (cid_sigs inst')
          }
      where
        nameOf = occNameString . rdrNameOcc

        bindName (L _ b) = case b of
          FunBind {fun_id = fid} -> nameOf (unLoc fid)
          _ -> ""

        -- InstanceSigs: the signature names are method references too.
        fixSigNames methodName (L _ orig) (L l new) = case (orig, new) of
          (TypeSig _ origNames _, TypeSig x _ ty) ->
            L l (TypeSig x (map (fmap (renameTo . methodName . nameOf)) origNames) ty)
          _ -> L l new
          where
            renameTo s = mkRdrUnqual (mkVarOcc s)

    -- Override the binder name of a method bind: fun_id and every
    -- equation's context name (the printer renders the latter).
    setMethodName :: String -> HsBind GhcPs -> HsBind GhcPs
    setMethodName name b = case b of
      FunBind {fun_id = fid, fun_matches = mg} ->
        b
          { fun_id = fmap retarget fid,
            fun_matches =
              mg {mg_alts = fmap (map (fmap fixMatch)) (mg_alts mg)}
          }
      _ -> b
      where
        retarget rdr =
          mkRdrUnqual (mkOccName (occNameSpace (rdrNameOcc rdr)) name)
        fixMatch m = case m_ctxt m of
          ctxt@(FunRhs {mc_fun = f}) ->
            m {m_ctxt = ctxt {mc_fun = fmap retarget f}}
          _ -> m

    -- The class named by an instance head: strip foralls, contexts,
    -- parens, and type applications down to the head type constructor.
    headClassName :: LHsSigType GhcPs -> Maybe (LocatedN RdrName)
    headClassName sigTy = headOf (sig_body (unLoc sigTy))
      where
        headOf lty = case unLoc lty of
          HsForAllTy {hst_body = body} -> headOf body
          HsQualTy {hst_body = body} -> headOf body
          HsParTy _ inner -> headOf inner
          HsAppTy _ f _ -> headOf f
          HsKindSig _ inner _ -> headOf inner
          HsTyVar _ _ n -> Just n
          _ -> Nothing

    resolveToModule = resolveRdrModule plan env

    -- Sequential scoping through a statement list, returning the scope
    -- after the last statement.
    threadStmts :: Shadow -> [ExprLStmt GhcPs] -> RM (Shadow, [ExprLStmt GhcPs])
    threadStmts sc [] = pure (sc, [])
    threadStmts sc (L l stmt : rest) = do
      (sc', stmt') <- case stmt of
        BindStmt x pat body -> do
          body' <- go sc body
          let scPat = sc <> binders collectPatBinders pat
          pat' <- go scPat pat
          pure (scPat, BindStmt x pat' body')
        LetStmt x binds -> do
          let sc' = sc <> binders collectLocalBinders binds
          binds' <- go sc' binds
          pure (sc', LetStmt x binds')
        -- TransStmt/ParStmt (advanced comprehensions) fall back to the
        -- generic case: adequate for their expressions, slightly coarse
        -- for their exotic scoping.
        other -> do
          other' <- gmapM (go sc) other
          pure (sc <> binders collectStmtBinders other', other')
      (scEnd, rest') <- threadStmts sc' rest
      pure (scEnd, L l stmt' : rest')

-- | Which local module an occurrence belongs to, if any: qualified via the
-- import alias, unqualified via the file's own module or its unqualified
-- local imports.
resolveRdrModule :: RenamePlan -> ResolveEnv -> RdrName -> Maybe ModuleName
resolveRdrModule plan env rdr = case rdr of
  Qual q _ -> Map.lookup q (reQualLocal env)
  Unqual occ
    | Just self <- reSelf env,
      Just p <- Map.lookup self (rpByModule plan),
      (nsKeyOf occ, occNameString occ) `Map.member` p ->
        Just self
    | otherwise ->
        case Map.lookup (nsKeyOf occ, occNameString occ) (reUnqualLocal env) of
          Just entries | [m] <- nubOrd (map fst entries) -> Just m
          _ -> Nothing
  _ -> Nothing

-- | Pre-pass: expand @C{..}@ (RecordWildCards) over local-module
-- constructors into explicit fields, because the implied binders/uses are
-- invisible to the parser and would defeat both shadow tracking and field
-- renaming. The synthesized labels carry their final (renamed) names; the
-- RHS variables keep the old field names, which is exactly what the
-- wildcard bound or referenced. Every textual edit is also recorded as a
-- patch (the @..@ span carries the synthesized fields).
expandWildcards ::
  RenamePlan ->
  Map ModuleName ModuleSymbols ->
  ResolveEnv ->
  LHsDecl GhcPs ->
  RM (LHsDecl GhcPs)
expandWildcards plan symsOf env =
  everywhereM (mkM patCase `extM` exprCase `extM` punPatCase `extM` punExprCase)
  where
    -- Puns over renamed fields must become explicit (@C{fA = f}@): the pun
    -- form would bind/reference a different variable after renaming, and
    -- the parser does not materialize a real pun RHS (so shadow tracking
    -- would miss the binder). The synthesized label carries its final
    -- name; the RHS keeps the old one.
    punPatCase ::
      HsFieldBind (LFieldOcc GhcPs) (LPat GhcPs) ->
      RM (HsFieldBind (LFieldOcc GhcPs) (LPat GhcPs))
    punPatCase = punCase varPatRhs

    punExprCase ::
      HsFieldBind (LFieldOcc GhcPs) (LHsExpr GhcPs) ->
      RM (HsFieldBind (LFieldOcc GhcPs) (LHsExpr GhcPs))
    punExprCase = punCase varExprRhs

    punCase ::
      (String -> arg) ->
      HsFieldBind (LFieldOcc GhcPs) arg ->
      RM (HsFieldBind (LFieldOcc GhcPs) arg)
    punCase mkRhs fld
      | hfbPun fld,
        let lbl = foLabel (unLoc (hfbLHS fld)),
        let rdr = unLoc lbl,
        let old = occNameString (rdrNameOcc rdr),
        Just m <- resolveRdrModule plan env rdr,
        Just new <- Map.lookup m (rpByModule plan) >>= Map.lookup (NsValue, old) = do
          patchAt (getLocA lbl) (new <> " = " <> old)
          pure
            fld
              { hfbLHS =
                  noLocA
                    ( FieldOcc
                        noExtField
                        (noLocA (mkRdrUnqual (mkOccName (occNameSpace (rdrNameOcc rdr)) new)))
                    ),
                hfbRHS = mkRhs old,
                hfbPun = False
              }
      | otherwise = pure fld
    patCase :: Pat GhcPs -> RM (Pat GhcPs)
    patCase p = case p of
      ConPat {pat_con = con, pat_args = RecCon flds}
        | Just (m, fields) <- localConFields (unLoc con) -> do
            flds' <- expandFlds varPatRhs m fields flds
            pure p {pat_args = RecCon flds'}
      _ -> pure p

    exprCase :: HsExpr GhcPs -> RM (HsExpr GhcPs)
    exprCase e = case e of
      RecordCon {rcon_con = con, rcon_flds = flds}
        | Just (m, fields) <- localConFields (unLoc con) -> do
            flds' <- expandFlds varExprRhs m fields flds
            pure e {rcon_flds = flds'}
      _ -> pure e

    localConFields :: RdrName -> Maybe (ModuleName, [String])
    localConFields rdr = do
      m <- resolveRdrModule plan env rdr
      syms <- Map.lookup m symsOf
      fields <- Map.lookup (occNameString (rdrNameOcc rdr)) (msFieldsOf syms)
      pure (m, fields)

    expandFlds ::
      (String -> arg) ->
      ModuleName ->
      [String] ->
      HsRecFields GhcPs arg ->
      RM (HsRecFields GhcPs arg)
    expandFlds mkRhs m fields hrf = do
      explicitFlds <- traverse fixExplicit (rec_flds hrf)
      case rec_dotdot hrf of
        Just ldd
          | not (null missing) ->
              patchAt
                (getHasLoc ldd)
                (intercalate ", " [newNameOf old <> " = " <> old | old <- missing])
        _ -> pure ()
      pure
        hrf
          { rec_flds = explicitFlds <> map synth missing,
            rec_dotdot = Nothing
          }
      where
        -- Explicit labels are resolved via the constructor, exactly like
        -- GHC does: with DisambiguateRecordFields an unqualified label is
        -- legal even when the module is imported qualified-only, so the
        -- lexical pass would miss it. Renaming happens here (the label
        -- becomes its final name and no longer matches any plan key); a
        -- still-punned field also gets its RHS materialized.
        fixExplicit lfld@(L l fld) =
          case (unLoc (foLabel (unLoc (hfbLHS fld))), planned) of
            (Unqual occ, Just planOf)
              | let old = occNameString occ,
                Just new <- Map.lookup (NsValue, old) planOf -> do
                  patchAt
                    (getLocA (foLabel (unLoc (hfbLHS fld))))
                    (if hfbPun fld then new <> " = " <> old else new)
                  pure . L l $
                    fld
                      { hfbLHS = finalLabel (occNameSpace occ) new,
                        hfbRHS = if hfbPun fld then mkRhs old else hfbRHS fld,
                        hfbPun = False
                      }
            _ -> pure lfld
        planned = Map.lookup m (rpByModule plan)

        explicit =
          Set.fromList
            [ occNameString (rdrNameOcc (unLoc (foLabel (unLoc (hfbLHS (unLoc f))))))
            | f <- rec_flds hrf
            ]
        missing = case rec_dotdot hrf of
          Nothing -> []
          Just _ -> filter (`Set.notMember` explicit) fields
        synth old =
          noLocA
            ( HsFieldBind
                noAnn
                (finalLabel (occNameSpace (mkVarOcc old)) (newNameOf old))
                (mkRhs old)
                False
            )
        newNameOf old = fromMaybe old (planned >>= Map.lookup (NsValue, old))
        finalLabel ns new =
          noLocA (FieldOcc noExtField (noLocA (mkRdrUnqual (mkOccName ns new))))

    varPatRhs old = noLocA (VarPat noExtField (noLocA (mkRdrUnqual (mkVarOcc old))))
    varExprRhs old = noLocA (HsVar noExtField (noLocA (mkRdrUnqual (mkVarOcc old))))
