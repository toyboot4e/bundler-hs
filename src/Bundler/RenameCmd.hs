module Bundler.RenameCmd
  ( Renamer
  , RenameQuery (..)
  , startRenamer
  , queryRenamer
  , stopRenamer
  ) where

import Bundler.Error
import Bundler.Symbols (isOperatorString)
import Control.Exception (IOException, try)
import Data.Char (isUpper)
import Data.List (intercalate)
import System.IO (BufferMode (..), Handle, hClose, hGetLine, hPutStrLn, hSetBuffering)
import System.Process.Typed

-- | One query to the user's rename command. Wire format is one
-- tab-separated line per query, one line (the new name) per response:
--
-- > kind \t module \t default-suffix \t name
--
-- Kinds: @value@, @type@, @con@, @field@, @op@, @extmod@. All fields are
-- non-empty: for @extmod@ the name is the module itself and the response is
-- the qualifier to use for it; for the rest the response is the new name.
-- A script reproduces the default behavior with @echo "$name$suffix"@
-- (and @echo "$name"@ for @op@/@extmod@).
data RenameQuery = RenameQuery
  { rqKind :: String
  , rqModule :: String
  , rqSuffix :: String
  , rqName :: String
  }

data Renamer = Renamer
  { rProcess :: Process Handle Handle ()
  , rCmd :: String
  }

startRenamer :: String -> IO Renamer
startRenamer cmd = do
  p <-
    startProcess
      (setStdin createPipe (setStdout createPipe (shell cmd)))
  hSetBuffering (getStdin p) LineBuffering
  hSetBuffering (getStdout p) LineBuffering
  pure Renamer {rProcess = p, rCmd = cmd}

-- | Lockstep request/response. Any I/O failure (child died, closed its
-- ends, short read) is a hard error.
queryRenamer :: Renamer -> RenameQuery -> IO (Either BundleError String)
queryRenamer r q = do
  result <- try $ do
    hPutStrLn
      (getStdin (rProcess r))
      (intercalate "\t" [rqKind q, rqModule q, rqSuffix q, rqName q])
    hGetLine (getStdout (rProcess r))
  pure $ case result of
    Left (e :: IOException) ->
      Left (RenameCmdError (rCmd r) ("no response for " <> rqName q <> ": " <> show e))
    Right response -> validateResponse q response

-- | Close the child's stdin (ending the stream) and require a clean exit.
stopRenamer :: Renamer -> IO (Either BundleError ())
stopRenamer r = do
  _ <- try @IOException (hClose (getStdin (rProcess r)))
  code <- waitExitCode (rProcess r)
  pure $ case code of
    ExitSuccess -> Right ()
    ExitFailure n ->
      Left (RenameCmdError (rCmd r) ("exited with code " <> show n))

-- | The response must be a plausible name of the same lexical class as the
-- original: operators stay symbolic, constructors capitalized, values
-- lowercase-ish. Anything else would only fail later with a confusing
-- parse error in the bundle.
validateResponse :: RenameQuery -> String -> Either BundleError String
validateResponse q new
  | null new = bad "empty response"
  | any (`elem` "\t ") new = bad "contains whitespace"
  | rqKind q == "extmod" = Right new
  | isOperatorString (rqName q) /= isOperatorString new =
      bad "operator/identifier class mismatch"
  | not (isOperatorString new)
  , mismatchedCase (rqName q) new =
      bad "capitalization class mismatch"
  | otherwise = Right new
  where
    mismatchedCase (o : _) (n : _) = isUpper o /= isUpper n
    mismatchedCase _ _ = False
    bad why =
      Left
        ( RenameCmdError
            ("response " <> show new <> " for " <> rqName q)
            why
        )
