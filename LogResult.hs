{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module LogResult where

import Data.Aeson
import Data.Default (Default (..))
import Data.List (stripPrefix)
import Data.Text (Text)
import GHC.Generics (Generic)
import Lens.Micro (Lens')

data Option = Option
  { optionName_ :: !Text
  , optionValue_ :: !Text
  }
  deriving (Eq, Ord, Show, Generic)

optionName :: Lens' Option Text
optionName f s = (\a -> s {optionName_ = a}) <$> f (optionName_ s)

optionValue :: Lens' Option Text
optionValue f s = (\a -> s {optionValue_ = a}) <$> f (optionValue_ s)

mkOption :: Text -> Text -> Option
mkOption = Option

data Failure = Failure
  { failureSelector_ :: !Option
  , failureSeed_ :: !Option
  }
  deriving (Eq, Ord, Show, Generic, Default)

failureSelector :: Lens' Failure Option
failureSelector f s = (\a -> s {failureSelector_ = a}) <$> f (failureSelector_ s)

failureSeed :: Lens' Failure Option
failureSeed f s = (\a -> s {failureSeed_ = a}) <$> f (failureSeed_ s)

mkFailure :: Option -> Option -> Failure
mkFailure = Failure

data SuiteRun = SuiteRun
  { suiteName_ :: !Text
  , suiteFailures_ :: ![Failure]
  }
  deriving (Eq, Ord, Show, Generic)

suiteName :: Lens' SuiteRun Text
suiteName f s = (\a -> s {suiteName_ = a}) <$> f (suiteName_ s)

suiteFailures :: Lens' SuiteRun [Failure]
suiteFailures f s = (\a -> s {suiteFailures_ = a}) <$> f (suiteFailures_ s)

mkSuiteRun :: Text -> [Failure] -> SuiteRun
mkSuiteRun = SuiteRun

data LogResult = LogResult
  { logPackage_ :: !Text
  , logSuites_ :: ![SuiteRun]
  }
  deriving (Eq, Ord, Show, Generic)

logPackage :: Lens' LogResult Text
logPackage f s = (\a -> s {logPackage_ = a}) <$> f (logPackage_ s)

logSuites :: Lens' LogResult [SuiteRun]
logSuites f s = (\a -> s {logSuites_ = a}) <$> f (logSuites_ s)

mkLogResult :: Text -> [SuiteRun] -> LogResult
mkLogResult = LogResult

instance ToJSON Option where
  toJSON = genericToJSON $ defaultOptions {fieldLabelModifier = relabel "option"}

instance ToJSON Failure where
  toJSON = genericToJSON $ defaultOptions {fieldLabelModifier = relabel "failure"}

instance ToJSON SuiteRun where
  toJSON = genericToJSON $ defaultOptions {fieldLabelModifier = relabel "suite"}

instance ToJSON LogResult where
  toJSON = genericToJSON $ defaultOptions {fieldLabelModifier = relabel "log"}

instance FromJSON Option where
  parseJSON = genericParseJSON $ defaultOptions {fieldLabelModifier = relabel "option"}

instance FromJSON Failure where
  parseJSON = genericParseJSON $ defaultOptions {fieldLabelModifier = relabel "failure"}

instance FromJSON SuiteRun where
  parseJSON = genericParseJSON $ defaultOptions {fieldLabelModifier = relabel "suite"}

instance FromJSON LogResult where
  parseJSON = genericParseJSON $ defaultOptions {fieldLabelModifier = relabel "log"}

relabel :: String -> String -> String
relabel p f = maybe f (camelTo2 '_') $ stripSuffix "_" =<< stripPrefix p f

stripSuffix :: Eq a => [a] -> [a] -> Maybe [a]
stripSuffix sfx = fmap reverse . stripPrefix (reverse sfx) . reverse

instance Default Option where
  def = Option {optionName_ = mempty, optionValue_ = mempty}

instance Default SuiteRun where
  def = SuiteRun {suiteName_ = mempty, suiteFailures_ = def}

instance Default LogResult where
  def = LogResult {logPackage_ = mempty, logSuites_ = def}
