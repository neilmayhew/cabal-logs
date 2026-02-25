{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Parse (parseLog) where

import Control.Exception (evaluate)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import LogResult
import System.FilePath (splitDirectories)
import Text.Regex.Applicative

cons :: a -> [a] -> [a]
!x `cons` !xs = let !y = x : xs in y

parseLog :: FilePath -> IO LogResult
parseLog fp = do
  infos <- mapMaybe findInfo . T.lines <$> T.readFile fp
  let package = case reverse (splitDirectories fp) of
        (_ : "test" : p : _) -> T.pack p
        _ -> "unknown-package"
  evaluate $ LogResult package (collectSuites infos)

collectSuites :: [LogInfo] -> [SuiteRun]
collectSuites = finalize . foldr go mempty
  where
    go :: LogInfo -> ([ReproInfo], [SuiteRun]) -> ([ReproInfo], [SuiteRun])
    go (SuiteName _) ([], suites) = ([], suites)
    go (SuiteName n) (infs, suites) = ([], mkSuiteRun n infs `cons` suites)
    go (ReproInfo inf) (infs, suites) = (inf `cons` infs, suites)
    finalize :: ([ReproInfo], [SuiteRun]) -> [SuiteRun]
    finalize ([], suites) = suites
    finalize (infs, suites) = mkSuiteRun "unknown-suite" infs `cons` suites
    mkSuiteRun :: Text -> [ReproInfo] -> SuiteRun
    mkSuiteRun n = SuiteRun n . collectFailures

collectFailures :: [ReproInfo] -> [Failure]
collectFailures = go
  where
    go (Seed seed : Selector sel : infs) =
      Failure sel seed `cons` go infs
    go (Selector sel : Seed seed : infs) =
      Failure sel seed `cons` go infs
    go (SelectorAndSeed sel seed : infs) =
      Failure sel seed `cons` go infs
    go (Selector sel : infs) =
      Failure sel def `cons` go infs
    go (Seed seed : infs) =
      Failure def seed `cons` go infs
    go [] = []
    def = Option "" ""

data LogInfo
  = SuiteName !Text
  | ReproInfo !ReproInfo
  deriving (Eq, Ord, Show)

data ReproInfo
  = Selector !Option
  | Seed !Option
  | SelectorAndSeed !Option !Option
  deriving (Eq, Ord, Show)

findInfo :: Text -> Maybe LogInfo
findInfo = fmap fst . findLongestPrefixWithUncons T.uncons (few anySym *> logInfo)

logInfo :: RE Char LogInfo
logInfo =
  asum
    [ SuiteName <$> suite
    , ReproInfo <$> reproInfo
    ]
  where
    suite = "Test suite " *> text <* ": RUNNING"

reproInfo :: RE Char ReproInfo
reproInfo =
  asum
    [ Selector <$ "Use " <*> (option "-p" <* " '" <*> text <* "'")
    , Seed <$ "Use " <*> (option "--quickcheck-replay" <* "=\"" <*> text <* "\"")
    , SelectorAndSeed
        <$ "To rerun use: "
        <*> (option "--match" <* " \"" <*> text <* "\" ")
        <*> (option "--seed" <* " " <*> text)
    ]
  where
    option name = Option . T.pack <$> name

text :: RE Char Text
text = T.pack <$> few anySym
