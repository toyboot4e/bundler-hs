module Bundler.Rename.Apply
  ( ResolveEnv (..)
  , mkResolveEnv
  , applyRenames
  ) where

import Bundler.Error
import Bundler.Parse
import Bundler.Rename.Plan
import Bundler.Symbols
import Data.Generics (Data, extM, gmapM)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import GHC.Hs
import GHC.Types.Name.Occurrence (mkOccName, occNameSpace, occNameString)
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
  -> ResolveEnv
  -> [LHsDecl GhcPs]
  -> Either BundleError [LHsDecl GhcPs]
applyRenames plan env = traverse (go Set.empty)
  where
    go :: Data a => Shadow -> a -> M a
    go sc =
      gen
        `extM` (rdrCase sc)
        `extM` (exprCase sc)
        `extM` (matchCase sc)
        `extM` (grhsCase sc)
        `extM` (bindCase sc)
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
