module Bundler.Minify
  ( minifyWith,
  )
where

import Bundler.Config (MinifyOptions (..))
import Bundler.Error
import Bundler.Parse (baseDynFlags)
import Data.Char (isSpace)
import Data.Foldable (toList)
import Data.Generics.Uniplate.DataOnly (universeBi)
import Data.List (dropWhileEnd, isPrefixOf, partition, sortOn)
import GHC.Data.FastString (mkFastString)
import GHC.Data.StringBuffer (stringToStringBuffer)
import GHC.Driver.Config.Parser (initParserOpts)
import GHC.Hs
import GHC.Parser.Lexer (ParseResult (..), Token (..), lexTokenStream)
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

-- | A position in the source, (line, column).
type Pos = (Int, Int)

-- | One output piece: start position (for ordering), priority at equal
-- positions, text, and end position (used to preserve token adjacency).
type Piece = (Pos, Int, String, Pos)

-- | Which section a top-level item belongs to, derived from the
-- @-- ### ...@ banner lines the assembler emits (they survive formatting,
-- being comments).
data Section = InUser | InLib
  deriving (Eq)

-- | Minify the selected sections of the bundle.
--
-- Minified declarations become one layout-free line each: layout cannot
-- be resolved by lexing alone (closing an implicit block at @in@ or
-- before @)@ needs parser feedback), so braces come from the AST's block
-- item spans, merged by position with the real tokens; tokens adjacent in
-- the source stay adjacent (prefix @\@@ @!@ @~@ @-@ are
-- whitespace-sensitive). Comments inside minified sections are dropped.
--
-- Non-minified declarations are emitted verbatim, which stays valid
-- inside the explicit-brace module block because item separators are
-- placed on their own lines, leaving the items' internal layout columns
-- untouched. Preserved CPP directives keep their own lines; separators
-- placed before each item mean a preprocessor-deleted branch takes its
-- separators with it.
minifyWith :: MinifyOptions -> String -> IO (Either BundleError String)
minifyWith opts src
  -- Only pragma combining requested: purely textual.
  | not (moLib opts || moUser opts || moImports opts) =
      pure (Right (unlines (combineLangPragmas pragmaBlock) <> unlines afterPragmas))
  | otherwise = do
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
        Right (emit modl (concatMap realToken toks))
  where
    origLines = lines src
    numberedLines = zip [1 :: Int ..] origLines
    directives = [(n, l) | (n, l) <- numberedLines, isDirectiveLine l]
    stripped =
      unlines [if isDirectiveLine l then "" else l | (_, l) <- numberedLines]
    isDirectiveLine l = take 1 l == "#"

    (pragmaBlock, afterPragmas) = span isHeaderLine origLines
    isHeaderLine l = null l || "{-#" `isPrefixOf` l

    combineLangPragmas ls
      | not (moPragmas opts) = filter (not . null) ls
      | otherwise =
          filter (not . null) others
            <> [ "{-# LANGUAGE " <> commas names <> " #-}"
               | not (null names)
               ]
      where
        (langs, others) = partition ("{-# LANGUAGE" `isPrefixOf`) ls
        names =
          [ name
          | l <- langs,
            name <- words (map decomma (takeWhile (/= '#') (drop 12 l)))
          ]
        decomma c = if c == ',' then ' ' else c
        commas [n] = n
        commas (n : ns) = n <> ", " <> commas ns
        commas [] = ""

    -- Per-line section, scanned from the assembler's banner lines.
    sectionAt :: Int -> Section
    sectionAt line =
      case [s | (n, s) <- bannerSections, n <= line] of
        [] -> InUser
        ss -> last ss
    bannerSections =
      [ (n, if "-- ### (user code)" `isPrefixOf` dropWhile isSpace l then InUser else InLib)
      | (n, l) <- numberedLines,
        "-- ###" `isPrefixOf` dropWhile isSpace l
      ]

    emit :: Located (HsModule GhcPs) -> [Piece] -> String
    emit modl tokens =
      unlines $
        combineLangPragmas pragmaBlock
          <> headerOut
          <> ["{"]
          <> body
          <> ["}"]
      where
        items =
          sortOn (\(pos, _, _) -> pos) $
            [ (start, end, True)
            | imp <- hsmodImports (unLoc modl),
              (start, end) <- realPos (getLocA imp)
            ]
              <> [ (start, end, False)
                 | decl <- hsmodDecls (unLoc modl),
                   (start, end) <- realPos (getLocA decl)
                 ]

        minified (startLine, _) isImport
          | isImport = moImports opts
          | otherwise = case sectionAt startLine of
              InUser -> moUser opts
              InLib -> moLib opts

        firstContentLine = case (items, directives) of
          ((s, _, _) : _, (d, _) : _) -> min (fst s) d
          ((s, _, _) : _, []) -> fst s
          ([], (d, _) : _) -> d
          ([], []) -> length origLines + 1

        -- The module header, minified to one line (it never hurts); a
        -- headerless file gets the header the Haskell report implies.
        headerOut = case hsmodName (unLoc modl) of
          Nothing -> ["module Main (main) where"]
          Just _ ->
            [ joinPieces
                [p | p@((line, _), _, _, _) <- pieces, line < firstContentLine]
            ]

        pieces = merge (blockMarks modl) tokens

        piecesWithin (start, end) =
          [p | p@(pos, _, _, _) <- pieces, pos >= start, pos < end || pos == end]

        -- Items and directives interleaved by line; the first item has no
        -- separator, every later one carries its own (inline for minified
        -- items, on its own line before verbatim ones so their layout
        -- columns survive).
        body = go True (sortOn entryLine stream)
          where
            stream =
              [Left d | d <- directives]
                <> [Right it | it <- items]
            entryLine (Left (n, _)) = n
            entryLine (Right ((n, _), _, _)) = n
            go _ [] = []
            go isFirst (Left (_, text) : rest) = text : go isFirst rest
            go isFirst (Right it@(start, end, isImport) : rest)
              | minified start isImport =
                  ((if isFirst then "" else "; ") <> joinPieces (piecesWithin (start, end)))
                    : go False rest
              | otherwise =
                  [";" | not isFirst]
                    <> verbatim it
                    <> go False rest
            verbatim ((startLine, _), (endLine, endCol), _) =
              [ line
              | line <-
                  take (endLine' - startLine + 1) (drop (startLine - 1) origLines)
              ]
              where
                endLine' = if endCol == 1 then endLine - 1 else endLine

    -- Single space between pieces, except between two tokens that were
    -- adjacent in the source.
    joinPieces :: [Piece] -> String
    joinPieces = dropWhileEnd isSpace . go Nothing
      where
        go _ [] = ""
        go prevEnd ((start, prio, text, end) : rest) =
          sep <> text <> go tokenEnd rest
          where
            sep = case prevEnd of
              Nothing -> ""
              Just pe
                | pe == start, prio == prioToken -> ""
                | otherwise -> " "
            tokenEnd
              | prio == prioToken = Just end
              -- A mark breaks adjacency but must still be followed by a
              -- space.
              | otherwise = Just (-1, -1)

    -- Real tokens only: virtual layout tokens are replaced by AST-derived
    -- marks, and comments (including pragmas, re-emitted separately) are
    -- dropped by constructor - textual sniffing would also hit legal
    -- operators like (-->).
    realToken :: GenLocated SrcSpan Token -> [Piece]
    realToken (L (RealSrcSpan real _) tok)
      | not (isCommentTok tok),
        srcSpanStartLine real == srcSpanEndLine real,
        srcSpanStartCol real < srcSpanEndCol real =
          [ ( (srcSpanStartLine real, srcSpanStartCol real),
              prioToken,
              sliceSpan real,
              (srcSpanEndLine real, srcSpanEndCol real)
            )
          ]
    realToken _ = []

    isCommentTok tok = case tok of
      ITlineComment {} -> True
      ITblockComment {} -> True
      ITdocComment {} -> True
      _ -> False

    sliceSpan real =
      take (srcSpanEndCol real - srcSpanStartCol real)
        . drop (srcSpanStartCol real - 1)
        $ case drop (srcSpanStartLine real - 1) origLines of
          (l : _) -> l
          [] -> ""

    -- Sort marks and tokens together by position; at equal positions
    -- closes come first, then separators, then opens, then the token
    -- starting there.
    merge :: [Piece] -> [Piece] -> [Piece]
    merge marks toks =
      sortOn (\(pos, prio, _, _) -> (pos, prio)) (marks <> toks)

mark :: Pos -> Int -> String -> Piece
mark pos prio text = (pos, prio, text, pos)

prioClose, prioSemi, prioOpen, prioToken :: Int
prioClose = 0
prioSemi = 1
prioOpen = 2
prioToken = 3

-- | Synthesized braces and separators for every layout block below the
-- top level, positioned at item boundaries. (The module block's braces
-- and item separators are handled by the emitter, which knows which
-- items are minified.)
blockMarks :: Located (HsModule GhcPs) -> [Piece]
blockMarks lmodl = concatMap marksFor blocks <> equationSemis
  where
    -- Consecutive equations of one function are a single declaration but
    -- separate items of the enclosing explicit-layout block: each
    -- equation after the first needs its own separator.
    equationSemis =
      [ mark pos prioSemi ";"
      | FunBind {fun_matches = mg} <-
          universeBi lmodl :: [HsBindLR GhcPs GhcPs],
        let alts = unLoc (mg_alts mg),
        length alts > 1,
        alt <- drop 1 alts,
        (pos, _) <- realPos (getLocA alt)
      ]

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
          [classItems c | c@(ClassDecl {}) <- universeBi lmodl :: [TyClDecl GhcPs]],
          -- GADT-syntax constructor lists (covers data family instances
          -- too, since all HsDataDefn nodes are visited).
          [ map getLocA (toList (dd_cons defn))
          | defn <- universeBi lmodl :: [HsDataDefn GhcPs],
            isGadtDefn defn
          ],
          -- Closed type family equations.
          [ map getLocA eqns
          | FamilyDecl {fdInfo = ClosedTypeFamily (Just eqns)} <-
              universeBi lmodl :: [FamilyDecl GhcPs]
          ],
          -- rec blocks in (m)do.
          [ map getLocA (unLoc ss)
          | RecStmt {recS_stmts = ss} <-
              universeBi lmodl :: [StmtLR GhcPs GhcPs (LHsExpr GhcPs)]
          ],
          -- Explicitly bidirectional pattern synonyms: the where block
          -- holds the builder's equations.
          [ map getLocA (unLoc (mg_alts mg))
          | PatSynBind _ (PSB {psb_dir = ExplicitBidirectional mg}) <-
              universeBi lmodl :: [HsBindLR GhcPs GhcPs]
          ],
          -- Arrow notation: command blocks mirror the expression ones but
          -- live in their own AST types.
          [ map getLocA stmts
          | HsCmdDo _ (L _ stmts) <- universeCmds
          ],
          [map getLocA (unLoc (mg_alts mg)) | HsCmdCase _ _ mg <- universeCmds],
          [map getLocA (unLoc (mg_alts mg)) | HsCmdLam _ style mg <- universeCmds, notSingleLam style],
          [localBindItems b | HsCmdLet _ b _ <- universeCmds],
          [ localBindItems (grhssLocalBinds g)
          | g <- universeBi lmodl :: [GRHSs GhcPs (LHsCmd GhcPs)]
          ],
          [ localBindItems b
          | LetStmt _ b <- universeBi lmodl :: [StmtLR GhcPs GhcPs (LHsCmd GhcPs)]
          ],
          [ map getLocA (unLoc ss)
          | RecStmt {recS_stmts = ss} <-
              universeBi lmodl :: [StmtLR GhcPs GhcPs (LHsCmd GhcPs)]
          ]
        ]

    universeExprs = universeBi lmodl :: [HsExpr GhcPs]
    universeCmds = universeBi lmodl :: [HsCmd GhcPs]

    notSingleLam LamSingle = False
    notSingleLam _ = True

    -- Comprehensions are HsDo too, but are bracket syntax, not layout.
    isRealDo flavour = case flavour of
      DoExpr _ -> True
      MDoExpr _ -> True
      _ -> False

    isGadtDefn defn = any (isGadtCon . unLoc) (toList (dd_cons defn))
      where
        isGadtCon (ConDeclGADT {}) = True
        isGadtCon _ = False

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
        <> map getLocA (tcdATDefs c)

    marksFor items = case sortOn fst (concatMap realPos items) of
      [] -> []
      spans@((start, _) : _) ->
        mark start prioOpen "{"
          : [mark pos prioSemi ";" | (pos, _) <- drop 1 spans]
            <> [mark (maximum (map snd spans)) prioClose "}"]

realPos :: SrcSpan -> [(Pos, Pos)]
realPos sp = case sp of
  RealSrcSpan real _ ->
    [ ( (srcSpanStartLine real, srcSpanStartCol real),
        (srcSpanEndLine real, srcSpanEndCol real)
      )
    ]
  _ -> []
