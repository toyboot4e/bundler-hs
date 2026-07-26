module Bundler.Symbols
  ( NsKey (..)
  , OccKey
  , SymKind (..)
  , ModuleSymbols (..)
  , moduleSymbols
  , nsKeyOf
  , occKeyOf
  , isOperatorString
  ) where

import Bundler.Parse
import Data.Char (isAlphaNum)
import Data.Foldable (toList)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import GHC.Hs
import GHC.Types.Name.Occurrence (OccName, isDataOcc, isTcOcc, occNameString)
import GHC.Types.Name.Reader (RdrName, rdrNameOcc)
import GHC.Types.SrcLoc (unLoc)

-- | GHC distinguishes several 'OccName' namespaces; for renaming we only
-- need three buckets. Record fields are bucketed with values so that a field
-- occurrence matches its binder regardless of whether the parser used the
-- dedicated field namespace.
data NsKey = NsValue | NsData | NsTcCls
  deriving (Eq, Ord, Show)

-- | What a top-level name is keyed by: namespace bucket + occurrence string.
type OccKey = (NsKey, String)

data SymKind
  = SymValue
  | SymField
  | SymDataCon
  | SymTyCon
  | SymClass
  | SymClassMethod
  | SymPatSyn
  deriving (Eq, Ord, Show)

-- | Every top-level binder of one module, plus the export/children structure
-- needed to interpret import and export lists.
data ModuleSymbols = ModuleSymbols
  { msAll :: Map OccKey SymKind
  -- ^ Every top-level binder, exported or not.
  , msExported :: Set OccKey
  -- ^ Subset of 'msAll' visible to importers.
  , msChildren :: Map String [OccKey]
  -- ^ Type/class name -> its constructors, fields, and methods (for
  -- @T(..)@ in import/export lists).
  , msFieldsOf :: Map String [String]
  -- ^ Data constructor name -> its record field names (for wildcard and
  -- pun expansion).
  }
  deriving (Show)

nsKeyOf :: OccName -> NsKey
nsKeyOf occ
  | isDataOcc occ = NsData
  | isTcOcc occ = NsTcCls
  | otherwise = NsValue

occKeyOf :: RdrName -> OccKey
occKeyOf rdr = (nsKeyOf occ, occNameString occ)
  where
    occ = rdrNameOcc rdr

-- | Operator names cannot take an alphanumeric suffix.
isOperatorString :: String -> Bool
isOperatorString = not . all (\c -> isAlphaNum c || c `elem` "_'")

-- | Collect all top-level symbols of a parsed module.
moduleSymbols :: ParsedFile -> ModuleSymbols
moduleSymbols pf =
  ModuleSymbols
    { msAll = allSyms
    , msExported = exported
    , msChildren = childrenMap
    , msFieldsOf = Map.fromListWith (<>) fieldsOf
    }
  where
    childrenMap = Map.fromListWith (<>) children
    modl = unLoc (pfModule pf)
    binders = concatMap (declBinders . unLoc) (hsmodDecls modl)
    allSyms = Map.fromList [(key, kind) | (key, kind, _, _) <- binders]
    children =
      [ (parent, [key])
      | (key, _, Just parent, _) <- binders
      ]
    fieldsOf =
      [ (con, [name])
      | ((_, name), SymField, _, Just con) <- binders
      ]
    exported = case hsmodExports modl of
      Nothing -> Map.keysSet allSyms
      Just ies ->
        Set.intersection (Map.keysSet allSyms) $
          Set.fromList (concatMap (exportedKeys . unLoc) (unLoc ies))

    -- Keys named by one export item. Non-name items (docs, sections) are
    -- ignored; module re-exports are rejected earlier in the pipeline.
    exportedKeys :: IE GhcPs -> [OccKey]
    exportedKeys ie = case ie of
      IEVar _ n _ -> [wrappedKey n]
      IEThingAbs _ n _ -> [wrappedKey n]
      IEThingAll _ n _ ->
        let key@(_, name) = wrappedKey n
         in key : fromMaybe [] (Map.lookup name childrenMap)
      IEThingWith _ n _ subs _ ->
        wrappedKey n : concatMap (bothNamespaces . wrappedKey) subs
      _ -> []

    wrappedKey :: LIEWrappedName GhcPs -> OccKey
    wrappedKey = occKeyOf . ieWrappedName . unLoc

    -- A sub-name in @T(a, B)@ may be a field (value bucket) or a data con;
    -- claim whichever exists.
    bothNamespaces :: OccKey -> [OccKey]
    bothNamespaces (_, name) =
      filter (`Map.member` allSyms) [(NsValue, name), (NsData, name)]

-- | Binders of one top-level declaration:
-- (key, kind, parent type/class name, owning data con for fields).
declBinders :: HsDecl GhcPs -> [(OccKey, SymKind, Maybe String, Maybe String)]
declBinders decl = case decl of
  ValD _ b -> bindBinders b
  TyClD _ tycl -> tyClBinders tycl
  ForD _ (ForeignImport {fd_name = n}) ->
    [(rdrKey n, SymValue, Nothing, Nothing)]
  _ -> []
  where
    rdrKey = occKeyOf . unLoc

    bindBinders b = case b of
      FunBind {fun_id = n} -> [(rdrKey n, SymValue, Nothing, Nothing)]
      PatBind {pat_lhs = p} ->
        [ (occKeyOf v, SymValue, Nothing, Nothing)
        | v <- collectPatBinders CollNoDictBinders p
        ]
      PatSynBind _ (PSB {psb_id = n, psb_args = details}) ->
        (rdrKey n, SymPatSyn, Nothing, Nothing)
          : [ (occKeyOf (unLoc (foLabel (recordPatSynField fld))), SymValue, Nothing, Nothing)
            | RecCon flds <- [details]
            , fld <- flds
            ]
      -- VarBind and trees-that-grow extension constructors.
      _ -> []

    tyClBinders tycl = case tycl of
      SynDecl {tcdLName = n} -> [(rdrKey n, SymTyCon, Nothing, Nothing)]
      FamDecl {tcdFam = FamilyDecl {fdLName = n}} ->
        [(rdrKey n, SymTyCon, Nothing, Nothing)]
      DataDecl {tcdLName = n, tcdDataDefn = defn} ->
        let tyName = snd (rdrKey n)
            cons = toList (dd_cons defn)
         in (rdrKey n, SymTyCon, Nothing, Nothing)
              : concatMap (conBinders tyName . unLoc) cons
      ClassDecl {tcdLName = n, tcdSigs = sigs, tcdATs = ats} ->
        let clsName = snd (rdrKey n)
         in (rdrKey n, SymClass, Nothing, Nothing)
              : [ (rdrKey m, SymClassMethod, Just clsName, Nothing)
                | ClassOpSig _ False ms _ <- map unLoc sigs
                , m <- ms
                ]
              <> [ (rdrKey (fdLName (unLoc at)), SymTyCon, Just clsName, Nothing)
                 | at <- ats
                 ]
      _ -> []

    conBinders tyName con = case con of
      ConDeclH98 {con_name = n, con_args = args} ->
        let conName = snd (rdrKey n)
         in (rdrKey n, SymDataCon, Just tyName, Nothing)
              : case args of
                RecCon flds -> concatMap (fieldBinders tyName conName . unLoc) (unLoc flds)
                _ -> []
      ConDeclGADT {con_names = ns, con_g_args = args} ->
        let conNames = map (snd . rdrKey) (toList ns)
         in [ (rdrKey n, SymDataCon, Just tyName, Nothing)
            | n <- toList ns
            ]
              <> case args of
                RecConGADT _ flds ->
                  concat
                    [ fieldBinders tyName conName (unLoc fld)
                    | fld <- unLoc flds
                    , conName <- conNames
                    ]
                _ -> []
      _ -> []

    fieldBinders tyName conName fld' = case fld' of
      ConDeclField {cd_fld_names = ns} ->
        [ ((NsValue, name), SymField, Just tyName, Just conName)
        | fld <- ns
        , let name = occNameString (rdrNameOcc (unLoc (foLabel (unLoc fld))))
        ]
      _ -> []
