-- | Apply span-anchored text replacements to source code. Used to rewrite
-- renamed references inside the user's own file so the bundle can carry it
-- verbatim - comments, blank lines, and formatting included - instead of a
-- pretty-printed reconstruction.
module Bundler.SourcePatch
  ( Patch,
    applyPatches,
  )
where

import Data.Containers.ListUtils (nubOrd)
import Data.List (sortOn)
import Data.Map.Strict qualified as Map
import GHC.Types.SrcLoc
  ( RealSrcSpan,
    srcSpanEndCol,
    srcSpanEndLine,
    srcSpanStartCol,
    srcSpanStartLine,
  )

-- | Replace the text under the span with the string.
type Patch = (RealSrcSpan, String)

-- | Apply all patches to the source. 'Nothing' when any patch spans
-- multiple lines or two patches overlap - the caller falls back to
-- pretty-printed output rather than risk mangled text.
applyPatches :: [Patch] -> String -> Maybe String
applyPatches patches src = do
  perLine <- traverse nonOverlapping byLine
  pure . unlines $
    [ maybe l (patchLine l) (Map.lookup n perLine)
    | (n, l) <- zip [1 :: Int ..] (lines src)
    ]
  where
    -- One occurrence may be patched identically via several routes (e.g.
    -- a binder both as fun_id and as the equation's context name).
    byLine =
      Map.fromListWith
        (<>)
        [(srcSpanStartLine sp, [p]) | p@(sp, _) <- nubOrd patches]

    nonOverlapping ps = do
      let sorted = sortOn (srcSpanStartCol . fst) ps
      mapM_ singleLine sorted
      mapM_ disjoint (zip sorted (drop 1 sorted))
      Just sorted
    singleLine (sp, _)
      | srcSpanStartLine sp == srcSpanEndLine sp = Just ()
      | otherwise = Nothing
    disjoint ((a, _), (b, _))
      | srcSpanEndCol a <= srcSpanStartCol b = Just ()
      | otherwise = Nothing

    -- Right-to-left so earlier columns stay valid.
    patchLine l ps =
      foldr
        (\(sp, new) acc -> splice (srcSpanStartCol sp) (srcSpanEndCol sp) new acc)
        l
        ps
    splice from to new l =
      take (from - 1) l <> new <> drop (to - 1) l
