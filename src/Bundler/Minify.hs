module Bundler.Minify
  ( minify,
  )
where

import Bundler.Error
import Bundler.Parse (baseDynFlags)
import Data.Generics.Uniplate.DataOnly (universeBi)
import Data.List (isPrefixOf, sortOn)
import GHC.Data.FastString (mkFastString)
import GHC.Data.StringBuffer (stringToStringBuffer)
import GHC.Driver.Config.Parser (initParserOpts)
import GHC.Hs
import GHC.Parser.Lexer (ParseResult (..), Token, lexTokenStream)
import GHC.Types.SrcLoc
  ( GenLocated (..),
    Located,
    RealSrcSpan,
    SrcSpan (..),
    mkRealSrcLoc,
    srcSpanEndCol,
    srcSpanEndLine,
    srcSpanStartCol,
    srcSpanStartLine,
    unLoc,
  )
import Language.Haskell.GhclibParserEx.GHC.Driver.Session (parsePragmasIntoDynFlags)
import Language.Haskell.GhclibParserEx.GHC.Parser (parseFile)

-- | Emit the bundle as a pragma block plus layout-free lines.
--
-- Layout cannot be resolved by lexing alone (closing an implicit block at
-- @in@ or before @)@ needs parser feedback), so the braces come from the
-- AST instead: every layout block's item spans are known after parsing,
-- and @{@ @;@ @}@ are synthesized at those positions, merged by source
-- position with the real tokens. Comments are dropped.
--
-- Preserved CPP directives (which sit between top-level declarations by
-- construction) survive: the code is emitted as one line per CPP-free
-- region with the directive lines in between. Separators are placed
-- before each item, so a branch deleted by the preprocessor takes its
-- @;@ with it and the surviving code stays valid.
minify :: String -> IO (Either BundleError String)
minify src = do
  parsedFlags <- parsePragmasIntoDynFlags baseDynFlags ([], []) "<minify>" stripped
  pure $ do
    flags <- either (Left . FormatCmdError "--minify") Right parsedFlags
    modl <- case parseFile "<minify>" flags stripped of
      POk _ m -> Right m
      PFailed _ -> Left (FormatCmdError "--minify" "the bundle does not re-parse")
    toks <- case lexTokenStream
      (initParserOpts flags)
      (stringToStringBuffer stripped)
      (mkRealSrcLoc (mkFastString "<minify>") 1 1) of
      POk _ ts -> Right ts
      PFailed _ -> Left (FormatCmdError "--minify" "the bundle does not re-lex")
    let (marks, topSpan) = blockMarks modl
        pieces = merge (marks <> topBraces topSpan) (concatMap realToken toks)
    Right (unlines pragmaLines <> regions pieces)
  where
    numberedLines = zip [1 :: Int ..] (lines src)
    directives = [(n, l) | (n, l) <- numberedLines, isDirectiveLine l]
    stripped =
      unlines [if isDirectiveLine l then "" else l | (_, l) <- numberedLines]
    srcLines = lines stripped
    pragmaLines =
      filter ("{-#" `isPrefixOf`) (takeWhile isHeaderLine srcLines)
    isHeaderLine l = null l || "{-#" `isPrefixOf` l
    isDirectiveLine l = take 1 l == "#"

    -- The module block's braces, hoisted out of conditional regions: the
    -- open goes just before the first directive when the first item is
    -- conditional, the close just after the last directive when the last
    -- item is. Directive lines carry no tokens, so those positions sort
    -- exactly at the region boundary. A branch deleted by the
    -- preprocessor can therefore never take a brace with it (item
    -- separators travel with their items on purpose). Other blocks are
    -- always within one declaration and thus one region.
    topBraces Nothing = []
    topBraces (Just (start@(startLine, _), end@(endLine, _))) =
      [(openPos, prioOpen, "{"), (closePos, prioClose, "}")]
      where
        openPos = case [d | (d, _) <- directives, d < startLine] of
          [] -> start
          (d1 : _) -> (d1, 0)
        closePos = case [d | (d, _) <- directives, d > endLine] of
          [] -> end
          ds -> (last ds + 1, 0)

    -- One minified line per CPP-free region, directive lines verbatim in
    -- between.
    regions pieces =
      concatMap emit (zip [0 ..] splitRegions)
      where
        splitRegions =
          [ [text | ((line, _), _, text) <- pieces, regionOf line == i]
          | i <- [0 .. length directives]
          ]
        regionOf line = length (takeWhile ((< line) . fst) directives)
        emit (i, ws) =
          (if i == 0 then "" else snd (directives !! (i - 1)) <> "\n")
            <> (if null ws then "" else unwords ws <> "\n")

    -- Real tokens only: virtual layout tokens are replaced by AST-derived
    -- marks, and comments (including pragmas, re-emitted separately) are
    -- dropped.
    realToken :: GenLocated SrcSpan Token -> [((Int, Int), Int, String)]
    realToken (L (RealSrcSpan real _) _)
      | srcSpanStartLine real == srcSpanEndLine real,
        srcSpanStartCol real < srcSpanEndCol real,
        let text = sliceSpan real,
        not (isComment text) =
          [((srcSpanStartLine real, srcSpanStartCol real), prioToken, text)]
    realToken _ = []

    isComment text = "--" `isPrefixOf` text || "{-" `isPrefixOf` text

    sliceSpan real =
      take (srcSpanEndCol real - srcSpanStartCol real)
        . drop (srcSpanStartCol real - 1)
        $ case drop (srcSpanStartLine real - 1) srcLines of
          (l : _) -> l
          [] -> ""

    -- Sort marks and tokens together by position; at equal positions
    -- closes come first, then separators, then opens, then the token
    -- starting there.
    merge marks toks = sortOn (\(pos, prio, _) -> (pos, prio)) (marks <> toks)

prioClose, prioSemi, prioOpen, prioToken :: Int
prioClose = 0
prioSemi = 1
prioOpen = 2
prioToken = 3

-- | Synthesized braces and separators for every layout block in the
-- module, positioned at item boundaries, plus the module block's own
-- (start, end) span — its braces are placed by the caller, which knows
-- about preserved CPP regions.
blockMarks ::
  Located (HsModule GhcPs) ->
  ([((Int, Int), Int, String)], Maybe ((Int, Int), (Int, Int)))
blockMarks lmodl = (concatMap marksFor blocks <> topSemis, topSpan)
  where
    modl = unLoc lmodl
    -- Imports and declarations together form the module's layout block.
    topLevel = map getLocA (hsmodImports modl) <> map getLocA (hsmodDecls modl)
    topItems = sortOn fst (concatMap realPos topLevel)
    topSemis = [(pos, prioSemi, ";") | (pos, _) <- drop 1 topItems]
    topSpan = case topItems of
      [] -> Nothing
      xs@((start, _) : _) -> Just (start, maximum (map snd xs))

    blocks :: [[SrcSpan]]
    blocks =
      concat
        [ [ map getLocA stmts
          | HsDo _ flavour (L _ stmts) <- universeExprs,
            isRealDo flavour
          ],
          [localBindItems b | HsLet _ b _ <- universeExprs],
          [ localBindItems b
          | LetStmt _ b <- universeBi lmodl :: [StmtLR GhcPs GhcPs (LHsExpr GhcPs)]
          ],
          [map getLocA (unLoc (mg_alts mg)) | HsCase _ _ mg <- universeExprs],
          [map getLocA (unLoc (mg_alts mg)) | HsLam _ style mg <- universeExprs, notSingleLam style],
          [map getLocA grhss | HsMultiIf _ grhss <- universeExprs],
          [ localBindItems (grhssLocalBinds g)
          | g <- universeBi lmodl :: [GRHSs GhcPs (LHsExpr GhcPs)]
          ],
          [instItems i | i <- universeBi lmodl :: [ClsInstDecl GhcPs]],
          [classItems c | c@(ClassDecl {}) <- universeBi lmodl :: [TyClDecl GhcPs]]
        ]

    universeExprs = universeBi lmodl :: [HsExpr GhcPs]

    notSingleLam LamSingle = False
    notSingleLam _ = True

    -- Comprehensions are HsDo too, but are bracket syntax, not layout.
    isRealDo flavour = case flavour of
      DoExpr _ -> True
      MDoExpr _ -> True
      _ -> False

    localBindItems (HsValBinds _ (ValBinds _ binds sigs)) =
      map getLocA binds <> map getLocA sigs
    localBindItems _ = []

    instItems i =
      map getLocA (cid_binds i)
        <> map getLocA (cid_sigs i)
        <> map getLocA (cid_tyfam_insts i)
        <> map getLocA (cid_datafam_insts i)

    classItems c =
      map getLocA (tcdSigs c)
        <> map getLocA (tcdMeths c)
        <> map getLocA (tcdATs c)

    marksFor items = case sortOn fst (concatMap realPos items) of
      [] -> []
      spans@((start, _) : _) ->
        ((start, prioOpen, "{"))
          : [(pos, prioSemi, ";") | (pos, _) <- drop 1 spans]
            <> [(maximum (map snd spans), prioClose, "}")]

    realPos sp = case sp of
      RealSrcSpan real _ ->
        [ ( (srcSpanStartLine real, srcSpanStartCol real),
            (srcSpanEndLine real, srcSpanEndCol real)
          )
        ]
      _ -> []
