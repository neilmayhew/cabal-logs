{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

import Cabal.Plan
import Control.Monad (guard, unless)
import qualified Data.Aeson as JSON
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Traversable (for)
import LogResults
import Options.Applicative hiding (Failure)
import Parse
import qualified System.Console.Terminal.Size as TS
import System.Directory (doesDirectoryExist, doesFileExist)
import System.Exit (die)
import System.FilePath ((<.>), (</>))
import System.IO (IOMode (WriteMode), hPutStrLn, stderr, withFile)

data Options = Options
  { optVerbosity :: Int
  , optProjectDir :: FilePath
  , optOutput :: FilePath
  , optOutputJson :: Bool
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
              optProjectDir <-
                strOption $
                  help "The project directory, or a subdirectory of it"
                    <> short 'p'
                    <> long "project"
                    <> metavar "DIR"
                    <> value "."
                    <> showDefaultWith id
              optOutput <-
                strOption $
                  help "Write output to FILE"
                    <> short 'o'
                    <> long "output"
                    <> metavar "FILE"
                    <> value "/dev/stdout"
                    <> showDefaultWith id
              optOutputJson <-
                switch $
                  help "Write output as JSON"
                    <> short 'j'
                    <> long "json"
              pure Options {..}
          )
          (fullDesc <> header "Extract failure information from Cabal test logs")
      )

  let trace n = if optVerbosity >= n then hPutStrLn stderr else const mempty

  -- Avoid confusing behaviour from `findProjectRoot`
  doesDirectoryExist optProjectDir
    >>= (`unless` die ("Project directory " <> optProjectDir <> " doesn't exist"))

  root <-
    findProjectRoot optProjectDir
      >>= maybe (die $ "Can't find project root in " <> optProjectDir) pure

  plan <- findAndDecodePlanJson $ ProjectRelativeToDir root

  let
    targetLogs = do
      -- List monad
      unit <- Map.elems $ pjUnits plan
      guard $ uType unit == UnitTypeLocal
      Just dir <- [uDistDir unit]
      comp@(CompNameTest tName) <- Map.keys (uComps unit)
      let
        pId = uPId unit
        PkgId pName _ = pId
        PkgName name = pName
        target = name <> ":" <> dispCompNameTarget pName comp
        file = dir </> "test" </> T.unpack (dispPkgId pId <> "-" <> tName) <.> "log"
      pure (target, file)

  trace 1 $ show (length targetLogs) <> " Cabal targets found"

  targetFailures <-
    for targetLogs $ \(target, file) -> do
      exists <- doesFileExist file
      failures <-
        if exists
          then do
            trace 2 $ "Examining " <> file
            parseLog file
          else
            pure mempty
      pure (target, failures)

  let
    logResults :: LogResults
    logResults = Map.fromList $ filter (not . null . snd) targetFailures

  trace 1 $ show (Map.size logResults) <> " logs with failures found"

  withFile optOutput WriteMode $ \h -> do
    if optOutputJson
      then do
        BSL.hPutStr h $ JSON.encode logResults
      else do
        unless (null logResults) $ do
          let
            prefix =
              [ "## Test Failures ##"
              , ""
              , "| Target | Seed | Pattern |"
              , "|:------ |:---- |:------- |"
              ]
            body =
              [ T.unwords ["|", target, "|", seed, "|", selector, "|"]
              | (target, failures) <- Map.toList logResults
              , f <- failures
              , let seed = optionValue (failureSeed f)
              , let selector = optionValue (failureSelector f)
              ]
          T.hPutStr h . T.unlines $ prefix <> body
