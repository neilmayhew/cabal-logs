{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

import Control.Monad (unless)
import Data.Foldable (for_)
import Data.List (sort)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Traversable (for)
import qualified Data.Yaml as Yaml
import Lens.Micro (SimpleGetter, (%~), (^.))
import Lens.Micro.Extras (view)
import LogResult
import Options.Applicative hiding (Failure)
import Parse
import qualified System.Console.Terminal.Size as TS
import System.IO (hPutStrLn, stderr)

data Options = Options
  { optVerbosity :: Int
  , optOutput :: Maybe FilePath
  , optLogFiles :: [FilePath]
  }
  deriving (Show)

main :: IO ()
main = do
  cols <- maybe 100 TS.width <$> TS.size

  let counter = fmap length . many . flag' ()
      strArguments mods = (:) <$> strArgument mods <*> many (strArgument mempty)

  Options {..} <-
    customExecParser
      (prefs $ columns cols)
      ( info
          ( helper <*> do
              optVerbosity <-
                counter $
                  help "Increase output verbosity (repeatable)"
                    <> short 'v'
                    <> long "verbose"
              optOutput <-
                optional . strOption $
                  help "Write YAML output to FILE for further analysis"
                    <> short 'o'
                    <> long "output"
                    <> metavar "FILE"
              optLogFiles <-
                strArguments $
                  help "Read the failures from FILE ..."
                    <> metavar "FILE ..."
              pure Options {..}
          )
          (fullDesc <> header "Extract failure information from Cabal test logs")
      )

  let
    trace n = if optVerbosity >= n then hPutStrLn stderr else const mempty
    nonEmptySuite = not . null . view suiteFailures
    nonEmptyLog = not . null . view logSuites
    forceSpine xs = foldr (\_ acc -> acc) () xs `seq` xs

  logResults <-
    fmap (forceSpine . filter nonEmptyLog . map (logSuites %~ filter nonEmptySuite)) $
      for optLogFiles $ \file -> do
        trace 1 $ "Examining " <> file
        parseLog file

  trace 1 $ show (length logResults) <> " logs with failures found"

  case optOutput of
    Just output -> do
      let
        options = Nothing `Yaml.setWidth` Yaml.defaultFormatOptions `Yaml.setFormat` Yaml.defaultEncodeOptions
        toMap :: Ord k => SimpleGetter s k -> SimpleGetter s v -> [s] -> Map k v
        toMap k v = Map.fromList . map (\s -> (s ^. k, s ^. v))
        toMaps = fmap (toMap suiteName suiteFailures) . toMap logPackage logSuites
      Yaml.encodeFileWith options output $ toMaps logResults
    Nothing -> do
      unless (null (logResults :: [LogResult])) $ do
        T.putStrLn "## Test Failures ##"
        T.putStrLn ""
        T.putStrLn "| Target | Seed | Pattern |"
        T.putStrLn "|:------ |:---- |:------- |"
      for_ (sort logResults) $ \lr -> do
        for_ (sort $ lr ^. logSuites) $ \sr -> do
          let target = lr ^. logPackage <> ":test:" <> sr ^. suiteName
          for_ (sr ^. suiteFailures) $ \f -> do
            T.putStrLn . T.unwords $
              ["|", target, "|", f ^. failureSeed . optionValue, "|", f ^. failureSelector . optionValue, "|"]
