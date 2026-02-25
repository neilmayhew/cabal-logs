{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

import Control.Monad (unless)
import Data.Foldable (for_)
import Data.List (sort)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Traversable (for)
import LogResult
import Options.Applicative hiding (Failure)
import Parse
import qualified System.Console.Terminal.Size as TS
import System.IO (IOMode (WriteMode), hPutStrLn, stderr, withFile)

data Options = Options
  { optVerbosity :: Int
  , optOutput :: FilePath
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
                strOption $
                  help "Write output to FILE"
                    <> short 'o'
                    <> long "output"
                    <> metavar "FILE"
                    <> value "/dev/stdout"
                    <> showDefaultWith id
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
    removeEmptyLogs = filter $ not . null . logSuites

  logResults <-
    fmap removeEmptyLogs $
      for optLogFiles $ \file -> do
        trace 1 $ "Examining " <> file
        parseLog file

  trace 1 $ show (length logResults) <> " logs with failures found"

  withFile optOutput WriteMode $ \h -> do
    unless (null logResults) $ do
      T.hPutStr h . T.unlines $
        [ "## Test Failures ##"
        , ""
        , "| Target | Seed | Pattern |"
        , "|:------ |:---- |:------- |"
        ]
    for_ (sort logResults) $ \lr -> do
      for_ (sort $ logSuites lr) $ \sr -> do
        let target = logPackage lr <> ":test:" <> suiteName sr
        for_ (suiteFailures sr) $ \f -> do
          T.hPutStrLn h . T.unwords $
            ["|", target, "|", optionValue (failureSeed f), "|", optionValue (failureSelector f), "|"]
