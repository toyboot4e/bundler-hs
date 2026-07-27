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

-- | Emit the bundle as a pragma block plus one layout-free line.
--
-- Layout cannot be resolved by lexing alone (closing an implicit block at
-- @in@ or before @)@ needs parser feedback), so the braces come from the
-- AST instead: every layout block's item spans are known after parsing,
-- and @{@ @;@ @}@ are synthesized at those positions, merged by source
-- position with the real tokens. Comments are dropped.
minify :: String -> IO (Either BundleError String)
minify src
  | any isDirectiveLine (lines src) =
      pure (Left (FormatCmdError "--minify" "preserved CPP directives cannot be minified"))
  | otherwise = do
      parsedFlags <- parsePragmasIntoDynFlags baseDynFlags ([], []) "<minify>" src
      pure $ do
        flags <- either (Left . FormatCmdError "--minify") Right parsedFlags
        modl <- case parseFile "<minify>" flags src of
          POk _ m -> Right m
          PFailed _ -> Left (FormatCmdError "--minify" "the bundle does not re-parse")
        toks <- case lexTokenStream
          (initParserOpts flags)
          (stringToStringBuffer src)
          (mkRealSrcLoc (mkFastString "<minify>") 1 1) of
          POk _ ts -> Right ts
          PFailed _ -> Left (FormatCmdError "--minify" "the bundle does not re-lex")
        let pieces = merge (blockMarks modl) (concatMap realToken toks)
        Right (unlines pragmaLines <> unwords pieces <> "\n")
  where
    srcLines = lines src
    pragmaLines =
      filter ("{-#" `isPrefixOf`) (takeWhile isHeaderLine srcLines)
    isHeaderLine l = null l || "{-#" `isPrefixOf` l
    isDirectiveLine l = take 1 l == "#"

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
    merge marks toks =
      [text | (_, _, text) <- sortOn (\(pos, prio, _) -> (pos, prio)) (marks <> toks)]

prioClose, prioSemi, prioOpen, prioToken :: Int
prioClose = 0
prioSemi = 1
prioOpen = 2
prioToken = 3

-- | Synthesized braces and separators for every layout block in the
-- module, positioned at item boundaries. For blocks closing at the same
-- position, the inner (later-starting) block closes first.
blockMarks :: Located (HsModule GhcPs) -> [((Int, Int), Int, String)]
blockMarks lmodl = concatMap marksFor (blocks <> [topLevel])
  where
    modl = unLoc lmodl
    -- Imports and declarations together form the module's layout block.
    topLevel = map getLocA (hsmodImports modl) <> map getLocA (hsmodDecls modl)

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
