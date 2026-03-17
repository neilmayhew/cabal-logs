{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

import Data.Either (partitionEithers)
import Data.Foldable (for_)
import Data.List (sort)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Traversable (for)
import LogResults
import Options.Applicative hiding (Failure)
import System.IO (hPutStrLn, stderr)

import Data.Aeson qualified as JSON
import Data.List.NonEmpty qualified as NE
import Data.Text qualified as T
import Data.Text.IO qualified as T
import System.Console.Terminal.Size qualified as TS

data Options = Options
  { optVerbosity :: Int
  , optOutput :: FilePath
  , optInputs :: [FilePath]
  }
  deriving (Show)

main :: IO ()
main = do
  cols <- maybe 100 TS.width <$> TS.size

  let counter = fmap length . many . flag' ()

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
                strOption $
                  help "Write output to FILE"
                    <> short 'o'
                    <> long "output"
                    <> metavar "FILE"
                    <> value "/dev/stdout"
                    <> showDefaultWith id
              optInputs <-
                many . strArgument $
                  help "JSON files containing failures"
                    <> metavar "FILE ..."
              pure Options {..}
          )
          (fullDesc <> header "Render failure information from Cabal test logs")
      )

  let trace n = if optVerbosity >= n then hPutStrLn stderr else const mempty

  trace 1 $ show (length optInputs) <> " extracted failures files found"

  (errs, inputs) <-
    fmap partitionEithers $
      for optInputs $ \input -> do
        trace 2 $ "Examining " <> input
        JSON.eitherDecodeFileStrict @LogResults input

  for_ errs $ hPutStrLn stderr

  let
    groupedFailures =
      NE.groupWith fst . sort $
        [ (suiteName, (optionValue failureSelector, logCompilerVersion, optionValue failureSeed))
        | LogResults {..} <- inputs
        , SuiteRun {..} <- logSuiteRuns
        , Failure {..} <- suiteFailures
        ]

  let
    prefix = ["## Test Failures ##"]
    body =
      concat
        [ [ ""
          , "### `" <> suite <> "` ###"
          , ""
          , "| Test                                         | Compiler | Seed     |"
          , "|:-------------------------------------------- |:-------- |:-------- |"
          ]
            <> [ T.unwords ["|", selector, "|", compilerName, "|", seed, "|"]
               | (selector, compiler, seed) <- map snd $ NE.toList g
               , let compilerName = T.intercalate "." $ map (T.pack . show) compiler
               ]
        | g@((suite, _) :| _) <- groupedFailures
        ]

  T.writeFile optOutput . T.unlines $ prefix <> body
