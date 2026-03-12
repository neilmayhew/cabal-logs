#!/usr/bin/env nix
#!nix shell nixpkgs#nix
#!nix github:tomberek/-#haskellWith.optparse-applicative.terminal-size.aeson.microlens.microlens-aeson.regex-applicative.process.mtl
#!nix -v -i -c runghc -Wall -Wcompat

{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

import Control.Monad (unless, when)
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson
import Data.ByteString (ByteString)
import Data.Char (toLower)
import Data.Foldable (for_)
import Data.List (sortOn, stripPrefix)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Time (NominalDiffTime, diffUTCTime, getCurrentTime)
import GHC.Generics (Generic)
import Lens.Micro
import Lens.Micro.Aeson
import Lens.Micro.Extras
import Options.Applicative
import System.Exit (ExitCode (..), exitFailure)
import System.IO (stderr)
import System.Process (readProcessWithExitCode, showCommandForUser)
import Text.Printf (printf)
import Text.Regex.Applicative

import qualified Data.ByteString as B
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified System.Console.Terminal.Size as TS

data Options = Options
  { optVerbose :: Bool
  , optProject :: FilePath
  , optFlakeLock :: FilePath
  }
  deriving (Show)

main :: IO ()
main = do
  cols <- maybe 100 TS.width <$> TS.size

  Options {..} <-
    customExecParser
      (prefs $ columns cols)
      ( info
          ( helper <*> do
              optVerbose <-
                switch $
                  help "Produce verbose output"
                    <> short 'v'
                    <> long "verbose"
              optProject <-
                strArgument $
                  help "Cabal project file"
                    <> metavar "FILE"
                    <> value "cabal.project"
              optFlakeLock <-
                strArgument $
                  help "Nix flake lock file"
                    <> metavar "FILE"
                    <> value "flake.lock"
              pure Options {..}
          )
          (fullDesc <> header "Check the nix hashes in a flake-enabled Cabal project")
      )

  locks <- parseLocks <$> B.readFile optFlakeLock

  srps <- parseSrps <$> T.readFile optProject

  failures <- checkHashes optVerbose (locks <> srps)

  for_ (sortOn snd failures) $ \(input, failure) -> do
    let urlAndRev = (input ^. inputUrl) <> "@" <> (input ^. inputRev)
    T.hPutStr stderr . T.unlines $
      case failure of
        NoSingleHash hashes ->
          [urlAndRev <> " has " <> (if null hashes then "no hashes" else "multiple hashes:")]
            <> map ("  " <>) hashes
        HashMismatch actual expected ->
          [ "Hash mismatch for " <> urlAndRev <> ":"
          , "  Specified: " <> expected
          , "     Actual: " <> actual
          ]
        PrefetchFailed code cmd args errs ->
          let cmd' = showCommandForUser (T.unpack cmd) (T.unpack <$> args)
           in [ "Prefetching failed for " <> urlAndRev <> ":"
              , "  " <> T.pack (show cmd') <> " exited with " <> T.pack (show code)
              ]
                <> map ("  " <>) (T.lines errs)

  unless (null failures) exitFailure

parseLocks :: ByteString -> [Input]
parseLocks = toListOf $ _Value . key "nodes" . members . key "locked" . _JSON

parseSrps :: Text -> [Input]
parseSrps = snd . foldr go (emptyInput, []) . T.lines
 where
  go l (cur, rest) =
    case T.words l of
      kw : _
        | kw == "source-repository-package" -> (emptyInput, cur : rest)
      kw : val : _
        | kw == "location:" -> (cur & inputUrl .~ val, rest)
        | kw == "tag:" -> (cur & inputRev .~ val, rest)
        | kw == "--sha256:" -> (cur & inputNarHash ?~ val, rest)
      _ -> (cur, rest)

data Failure
  = NoSingleHash ![Text]
  | HashMismatch !Text !Text
  | PrefetchFailed !Int !Text ![Text] !Text
  deriving (Eq, Ord, Show)

checkHashes :: Bool -> [Input] -> IO [(Input, Failure)]
checkHashes verbose inputs = do
  let
    inputCommit Input {..} = (inputType_, inputOwner_, inputRepo_, inputRev_)
    groups = NE.groupAllWith inputCommit inputs
    checkGroup group = do
      let
        input = NE.head group
        urlAndRev = (input ^. inputUrl) <> "@" <> (input ^. inputRev)
        hashes = mapMaybe (view inputNarHash) $ NE.toList group
      when verbose $
        T.putStr $
          "Checking " <> urlAndRev <> " ..."
      (duration, failures) <-
        timed $
          case hashes of
            [hash] -> do
              checkHash input hash
            _ -> do
              pure [NoSingleHash hashes]
      when verbose $
        printf " %.2fs\n" (realToFrac duration :: Double)
      pure $ (input,) <$> failures

  foldMap checkGroup groups

checkHash :: Input -> Text -> IO [Failure]
checkHash inp expectedHash = do
  let
    owner = inp ^. inputOwner
    repo = inp ^. inputRepo
    rev = inp ^. inputRev
    archiveUrl =
      case inp ^. inputType of
        "github" ->
          T.intercalate
            "/"
            ["https://github.com", owner, repo, "archive", rev <> ".tar.gz"]
        "gitlab" ->
          T.intercalate
            "/"
            ["https://gitlab.com", owner, repo, "-", "archive", repo <> "-" <> rev <> ".tar.gz"]
        typ ->
          error "Unknown input type: " <> typ

  result <- runExceptT $ do
    (T.strip -> hash32) <-
      readProcessExceptT "nix-prefetch-url" ["--unpack", archiveUrl] ""
    (T.strip -> actualHash) <-
      readProcessExceptT "nix" ["hash", "convert", "--hash-algo", "sha256", hash32] ""
    pure [HashMismatch actualHash expectedHash | actualHash /= expectedHash]

  case result of
    Left (code, cmd, args, err) -> pure [PrefetchFailed code cmd args err]
    Right mismatches -> pure mismatches

readProcessExceptT :: Text -> [Text] -> Text -> ExceptT (Int, Text, [Text], Text) IO Text
readProcessExceptT cmd args input = do
  (result, out, err) <-
    liftIO $ readProcessWithExitCode (T.unpack cmd) (T.unpack <$> args) (T.unpack input)
  case result of
    ExitSuccess ->
      pure $ T.pack out
    ExitFailure code -> do
      throwError (code, cmd, args, T.pack err)

timed :: IO a -> IO (NominalDiffTime, a)
timed act = do
  start <- getCurrentTime
  result <- act
  end <- getCurrentTime
  pure (end `diffUTCTime` start, result)

data Input = Input
  { inputType_ :: !Text
  , inputOwner_ :: !Text
  , inputRepo_ :: !Text
  , inputRev_ :: !Text
  , inputNarHash_ :: !(Maybe Text)
  }
  deriving (Eq, Ord, Show, Generic)

inputType :: Lens' Input Text
inputType f s = (\a -> s {inputType_ = a}) <$> f (inputType_ s)

inputOwner :: Lens' Input Text
inputOwner f s = (\a -> s {inputOwner_ = a}) <$> f (inputOwner_ s)

inputRepo :: Lens' Input Text
inputRepo f s = (\a -> s {inputRepo_ = a}) <$> f (inputRepo_ s)

inputRev :: Lens' Input Text
inputRev f s = (\a -> s {inputRev_ = a}) <$> f (inputRev_ s)

inputNarHash :: Lens' Input (Maybe Text)
inputNarHash f s = (\a -> s {inputNarHash_ = a}) <$> f (inputNarHash_ s)

-- This isn't a fully lawful lens because you may not get back out what you put in
-- However, it does normalize URLs
inputUrl :: Lens' Input Text
inputUrl f s = setter s <$> f (getter s)
 where
  getter Input {..} = "https://" <> inputType_ <> ".com/" <> inputOwner_ <> "/" <> inputRepo_ <> ".git"
  re = (,,) <$ "https://" <*> text <* ".com/" <*> text <* "/" <*> text <* ("/" <|> ".git" <|> "")
  setter inp url =
    case match re . T.unpack $ url of
      Just (t, o, r) -> inp {inputType_ = t, inputOwner_ = o, inputRepo_ = r}
      Nothing -> inp
  text = T.pack <$> few anySym

emptyInput :: Input
emptyInput = Input "" "" "" "" Nothing

instance ToJSON Input where
  toJSON = genericToJSON $ defaultOptions {fieldLabelModifier = relabel "input"}

instance FromJSON Input where
  parseJSON = genericParseJSON $ defaultOptions {fieldLabelModifier = relabel "input"}

relabel :: String -> String -> String
relabel p f = maybe f (_head %~ toLower) $ stripPrefix p =<< (f ^? _init)
