{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

import Cabal.Plan
import Control.Monad (guard, unless)
import Data.Bool (bool)
import Data.Foldable (for_)
import Data.List (sort)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Traversable (for)
import LogResult
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
              pure Options {..}
          )
          (fullDesc <> header "Extract failure information from Cabal test logs")
      )

  -- Avoid confusing behaviour from `findProjectRoot`
  doesDirectoryExist optProjectDir
    >>= bool (die $ "Project directory " <> optProjectDir <> " doesn't exist") (pure ())

  root <-
    findProjectRoot optProjectDir
      >>= maybe (die $ "Can't find project root in " <> optProjectDir) pure

  plan <- findAndDecodePlanJson $ ProjectRelativeToDir root

  let
    logs = do
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
    trace n = if optVerbosity >= n then hPutStrLn stderr else const mempty
    removeEmptyLogs = filter $ not . null . snd

  logResults <-
    fmap removeEmptyLogs $
      for logs $ \(target, file) -> do
        exists <- doesFileExist file
        failures <-
          if exists
            then do
              trace 1 $ "Examining " <> file
              parseLog file
            else
              pure mempty
        pure (target, failures)

  trace 1 $ show (length logResults) <> " logs with failures found"

  withFile optOutput WriteMode $ \h -> do
    unless (null logResults) $ do
      T.hPutStr h . T.unlines $
        [ "## Test Failures ##"
        , ""
        , "| Target | Seed | Pattern |"
        , "|:------ |:---- |:------- |"
        ]
    for_ (sort logResults) $ \(target, failures) -> do
      for_ failures $ \f -> do
        T.hPutStrLn h . T.unwords $
          ["|", target, "|", optionValue (failureSeed f), "|", optionValue (failureSelector f), "|"]
