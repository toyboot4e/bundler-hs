module Bundler.Rename.Apply
  ( ResolveEnv (..)
  , mkResolveEnv
  , applyRenames
  ) where

import Bundler.Error
import Bundler.Parse
import Bundler.Rename.Plan
import Bundler.Symbols
import Data.Generics (Data, everywhereM, extM, gmapM, mkM)
import Data.List (foldl')
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import GHC.Hs
import GHC.Types.Name.Occurrence
  ( mkOccName
  , mkVarOcc
  , occNameSpace
  , occNameString
  , varName
  )
import GHC.Types.Name.Reader (RdrName (..), mkRdrUnqual, rdrNameOcc)
import GHC.Types.SrcLoc (GenLocated (..), unLoc)
import Language.Haskell.Syntax.Module.Name (ModuleName, moduleNameString)

type M = Either BundleError

-- | How names written in one particular file resolve to local modules.
data ResolveEnv = ResolveEnv
  { reSelf :: Maybe ModuleName
  -- ^ The module the rewritten file itself defines ('Nothing' for the
  -- user's file, whose own names are never renamed).
  , reQualLocal :: Map ModuleName ModuleName
  -- ^ Written qualifier (alias or module name) -> local module.
  , reUnqualLocal :: Map OccKey [(ModuleName, String)]
  -- ^ Names reachable unqualified via imports of local modules -> their
  -- planned new names (several entries = ambiguous if actually used).
  }

-- | Build the environment for one file from its imports of local modules.
-- A non-qualified import also allows qualified access under the same name,
-- so every local import contributes its qualifier; non-qualified imports
-- additionally bring (a subset of) the module's exports into unqualified
-- scope, honoring explicit and @hiding@ lists.
mkResolveEnv
  :: RenamePlan
  -> Map ModuleName ModuleSymbols
  -> Maybe ModuleName
  -> ParsedFile
  -> Either BundleError ResolveEnv
mkResolveEnv plan symsOf self pf = do
  unqual <- mconcat <$> traverse (unqualsOf . unLoc) imports
  pure
    ResolveEnv
      { reSelf = self
      , reQualLocal =
          Map.fromList
            [ (maybe m unLoc (ideclAs imp), m)
            | imp <- map unLoc imports
            , let m = unLoc (ideclName imp)
            , m `Map.member` symsOf
            ]
      , reUnqualLocal = Map.unionsWith (<>) unqual
      }
  where
    imports = hsmodImports (unLoc (pfModule pf))

    unqualsOf :: ImportDecl GhcPs -> M [Map OccKey [(ModuleName, String)]]
    unqualsOf imp
      | NotQualified <- ideclQualified imp
      , Just syms <- Map.lookup m symsOf = do
          keys <- visibleKeys syms
          pure
            [ Map.fromList
                [ (key, [(m, new)])
                | key <- Set.toList keys
                , Just new <- [lookupPlanned key]
                ]
            ]
      | otherwise = pure []
      where
        m = unLoc (ideclName imp)
        lookupPlanned key = Map.lookup m (rpByModule plan) >>= Map.lookup key

        visibleKeys syms = case ideclImportList imp of
          Nothing -> pure (msExported syms)
          Just (interp, L _ items) -> do
            listed <- Set.fromList . concat <$> traverse (itemKeys syms . unLoc) items
            pure $ case interp of
              Exactly -> listed
              EverythingBut -> msExported syms `Set.difference` listed

        -- Keys named by one import-list item, checked against the module's
        -- exports so typos and private names fail loudly here rather than
        -- silently staying unrenamed.
        itemKeys :: ModuleSymbols -> IE GhcPs -> M [OccKey]
        itemKeys syms ie = case ie of
          IEVar _ n _ -> checked [wrappedKey n]
          IEThingAbs _ n _ -> checked [wrappedKey n]
          IEThingAll _ n _ ->
            let key@(_, name) = wrappedKey n
             in checked (key : Map.findWithDefault [] name (msChildren syms))
          IEThingWith _ n _ subs _ ->
            checked (wrappedKey n : concatMap (claim syms . wrappedKey) subs)
          _ -> pure []
          where
            checked keys = case filter (`Set.notMember` msExported syms) keys of
              [] -> pure keys
              (_, name) : _ -> Left (NotExported name (moduleNameString m))
        wrappedKey = occKeyOf . ieWrappedName . unLoc
        claim syms (_, name) =
          filter (`Map.member` msAll syms) [(NsValue, name), (NsData, name)]

-- | Names currently shadowed by enclosing lambda/let/where/case/do/
-- comprehension binders.
type Shadow = Set OccKey

-- | Rewrite every 'RdrName' occurrence in the declarations according to the
-- plan, tracking local scope so shadowed names stay untouched.
--
-- The traversal is generic ('gmapM') with type-specific cases for every
-- construct that introduces binders. Binders scope over sibling subtrees
-- (e.g. pattern binders over the equation body), which is why this cannot
-- be a plain @everywhereM@.
applyRenames
  :: RenamePlan
  -> Map ModuleName ModuleSymbols
  -> ResolveEnv
  -> [LHsDecl GhcPs]
  -> Either BundleError [LHsDecl GhcPs]
applyRenames plan symsOf env decls = do
  expanded <- traverse (expandWildcards plan symsOf env) decls
  traverse (go Set.empty) expanded
  where
    go :: Data a => Shadow -> a -> M a
    go sc =
      gen
        `extM` (rdrCase sc)
        `extM` (exprCase sc)
        `extM` (matchCase sc)
        `extM` (grhsCase sc)
        `extM` (bindCase sc)
        `extM` (instCase sc)
      where
        gen :: Data d => d -> M d
        gen = gmapM (go sc)

    binders :: (CollectFlag GhcPs -> a -> [IdP GhcPs]) -> a -> Shadow
    binders collect x =
      Set.fromList (map occKeyOf (collect CollNoDictBinders x))

    -- Leaf rewrite. Qualified references cannot be shadowed; unqualified
    -- ones consult the shadow set first, then the module's own top level,
    -- then unqualified imports of local modules.
    rdrCase :: Shadow -> RdrName -> M RdrName
    rdrCase sc rdr = case rdr of
      Unqual occ
        | occKeyOf rdr `Set.member` sc -> Right rdr
        | Just self <- reSelf env
        , Just new <- lookupPlan self (occKeyOf rdr) ->
            Right (unqual occ new)
        | otherwise -> case Map.lookup (occKeyOf rdr) (reUnqualLocal env) of
            Nothing -> Right rdr
            Just [(_, new)] -> Right (unqual occ new)
            Just several ->
              Left
                ( AmbiguousName
                    (occNameString occ)
                    (map (moduleNameString . fst) several)
                )
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

    lookupPlan m key = Map.lookup m (rpByModule plan) >>= Map.lookup key
    unqual occ new = mkRdrUnqual (mkOccName (occNameSpace occ) new)

    -- One equation/alternative: pattern binders scope over the patterns
    -- themselves (view patterns, and protecting the binder occurrences),
    -- the guards, the RHS, and the where block.
    matchCase :: Shadow -> Match GhcPs (LHsExpr GhcPs) -> M (Match GhcPs (LHsExpr GhcPs))
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
    grhsCase :: Shadow -> GRHS GhcPs (LHsExpr GhcPs) -> M (GRHS GhcPs (LHsExpr GhcPs))
    grhsCase sc (GRHS x guards body) = do
      (sc', guards') <- threadStmts sc guards
      body' <- go sc' body
      pure (GRHS x guards' body')

    -- A pattern binding's where block scopes over guards and RHS. (The
    -- pattern's own binders belong to the enclosing scope and are handled
    -- by whoever collected them - top level or let/where.)
    bindCase :: Shadow -> HsBind GhcPs -> M (HsBind GhcPs)
    bindCase sc b = case b of
      PatBind {pat_lhs = lhs, pat_rhs = grhss} -> do
        let scAll = sc <> binders collectLocalBinders (grhssLocalBinds grhss)
        lhs' <- go sc lhs
        grhss' <- go scAll grhss
        pure b {pat_lhs = lhs', pat_rhs = grhss'}
      _ -> gmapM (go sc) b

    exprCase :: Shadow -> HsExpr GhcPs -> M (HsExpr GhcPs)
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
    -- names are overridden afterwards.
    instCase :: Shadow -> ClsInstDecl GhcPs -> M (ClsInstDecl GhcPs)
    instCase sc inst = do
      inst' <- gmapM (go sc) inst
      let clsPlan = do
            cls <- headClassName (cid_poly_ty inst)
            m <- resolveToModule (unLoc cls)
            Map.lookup m (rpByModule plan)
          methodName old = case clsPlan of
            Nothing -> old
            Just p -> fromMaybe old (Map.lookup ((NsValue, old)) p)
          fixOne orig (L l b) = L l (setMethodName (methodName orig) b)
      pure
        inst'
          { cid_binds =
              zipWith
                fixOne
                (map bindName (cid_binds inst))
                (cid_binds inst')
          , cid_sigs =
              zipWith
                (fixSigNames methodName)
                (cid_sigs inst)
                (cid_sigs inst')
          }
      where
        bindName (L _ b) = case b of
          FunBind {fun_id = fid} -> occNameString (rdrNameOcc (unLoc fid))
          _ -> ""

        -- InstanceSigs: the signature names are method references too.
        fixSigNames methodName (L _ orig) (L l new) = case (orig, new) of
          (TypeSig _ origNames _, TypeSig x _ ty) ->
            L l (TypeSig x (map (fmap (renameTo . methodName . nameOf)) origNames) ty)
          _ -> L l new
          where
            nameOf = occNameString . rdrNameOcc
            renameTo s = mkRdrUnqual (mkVarOcc s)

    -- Override the binder name of a method bind: fun_id and every
    -- equation's context name (the printer renders the latter).
    setMethodName :: String -> HsBind GhcPs -> HsBind GhcPs
    setMethodName name b = case b of
      FunBind {fun_id = fid, fun_matches = mg} ->
        b
          { fun_id = fmap retarget fid
          , fun_matches =
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
    threadStmts :: Shadow -> [ExprLStmt GhcPs] -> M (Shadow, [ExprLStmt GhcPs])
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
    | Just self <- reSelf env
    , Just p <- Map.lookup self (rpByModule plan)
    , (nsKeyOf occ, occNameString occ) `Map.member` p ->
        Just self
    | otherwise ->
        case Map.lookup (nsKeyOf occ, occNameString occ) (reUnqualLocal env) of
          Just [(m, _)] -> Just m
          _ -> Nothing
  _ -> Nothing

-- | Pre-pass: expand @C{..}@ (RecordWildCards) over local-module
-- constructors into explicit fields, because the implied binders/uses are
-- invisible to the parser and would defeat both shadow tracking and field
-- renaming. The synthesized labels carry their final (renamed) names; the
-- RHS variables keep the old field names, which is exactly what the
-- wildcard bound or referenced.
expandWildcards
  :: RenamePlan
  -> Map ModuleName ModuleSymbols
  -> ResolveEnv
  -> LHsDecl GhcPs
  -> Either BundleError (LHsDecl GhcPs)
expandWildcards plan symsOf env =
  everywhereM (mkM patCase `extM` exprCase `extM` punPatCase `extM` punExprCase)
  where
    -- Puns over renamed fields must become explicit (@C{fA = f}@): the pun
    -- form would bind/reference a different variable after renaming, and
    -- the parser does not materialize a real pun RHS (so shadow tracking
    -- would miss the binder). The synthesized label carries its final
    -- name; the RHS keeps the old one.
    punPatCase
      :: HsFieldBind (LFieldOcc GhcPs) (LPat GhcPs)
      -> Either BundleError (HsFieldBind (LFieldOcc GhcPs) (LPat GhcPs))
    punPatCase = punCase varPatRhs

    punExprCase
      :: HsFieldBind (LFieldOcc GhcPs) (LHsExpr GhcPs)
      -> Either BundleError (HsFieldBind (LFieldOcc GhcPs) (LHsExpr GhcPs))
    punExprCase = punCase varExprRhs

    punCase
      :: (String -> arg)
      -> HsFieldBind (LFieldOcc GhcPs) arg
      -> Either BundleError (HsFieldBind (LFieldOcc GhcPs) arg)
    punCase mkRhs fld
      | hfbPun fld
      , let rdr = unLoc (foLabel (unLoc (hfbLHS fld)))
      , let old = occNameString (rdrNameOcc rdr)
      , Just m <- resolveRdrModule plan env rdr
      , Just new <- Map.lookup m (rpByModule plan) >>= Map.lookup (NsValue, old) =
          pure
            fld
              { hfbLHS =
                  noLocA
                    ( FieldOcc
                        noExtField
                        (noLocA (mkRdrUnqual (mkOccName (occNameSpace (rdrNameOcc rdr)) new)))
                    )
              , hfbRHS = mkRhs old
              , hfbPun = False
              }
      | otherwise = pure fld
    patCase :: Pat GhcPs -> Either BundleError (Pat GhcPs)
    patCase p = case p of
      ConPat {pat_con = con, pat_args = RecCon flds}
        | Just (m, fields) <- localConFields (unLoc con) ->
            pure p {pat_args = RecCon (expandFlds varPatRhs m fields flds)}
      _ -> pure p

    exprCase :: HsExpr GhcPs -> Either BundleError (HsExpr GhcPs)
    exprCase e = case e of
      RecordCon {rcon_con = con, rcon_flds = flds}
        | Just (m, fields) <- localConFields (unLoc con) ->
            pure e {rcon_flds = expandFlds varExprRhs m fields flds}
      _ -> pure e

    localConFields :: RdrName -> Maybe (ModuleName, [String])
    localConFields rdr = do
      m <- resolveRdrModule plan env rdr
      syms <- Map.lookup m symsOf
      fields <- Map.lookup (occNameString (rdrNameOcc rdr)) (msFieldsOf syms)
      pure (m, fields)

    expandFlds
      :: (String -> arg)
      -> ModuleName
      -> [String]
      -> HsRecFields GhcPs arg
      -> HsRecFields GhcPs arg
    expandFlds mkRhs m fields hrf = case rec_dotdot hrf of
      Nothing -> hrf
      Just _ ->
        hrf
          { rec_flds = rec_flds hrf <> map synth missing
          , rec_dotdot = Nothing
          }
      where
        explicit =
          Set.fromList
            [ occNameString (rdrNameOcc (unLoc (foLabel (unLoc (hfbLHS (unLoc f))))))
            | f <- rec_flds hrf
            ]
        missing = filter (`Set.notMember` explicit) fields
        synth old =
          noLocA
            ( HsFieldBind
                noAnn
                (noLocA (FieldOcc noExtField (noLocA (mkRdrUnqual (mkVarOcc (newNameOf old))))))
                (mkRhs old)
                False
            )
        newNameOf old =
          fromMaybe old $
            Map.lookup m (rpByModule plan) >>= Map.lookup (NsValue, old)

    varPatRhs old = noLocA (VarPat noExtField (noLocA (mkRdrUnqual (mkVarOcc old))))
    varExprRhs old = noLocA (HsVar noExtField (noLocA (mkRdrUnqual (mkVarOcc old))))
