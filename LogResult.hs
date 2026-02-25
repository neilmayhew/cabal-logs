module LogResult where

import Data.Text (Text)

data Option = Option
  { optionName :: !Text
  , optionValue :: !Text
  }
  deriving (Eq, Ord, Show)

data Failure = Failure
  { failureSelector :: !Option
  , failureSeed :: !Option
  }
  deriving (Eq, Ord, Show)

data SuiteRun = SuiteRun
  { suiteName :: !Text
  , suiteFailures :: ![Failure]
  }
  deriving (Eq, Ord, Show)

data LogResult = LogResult
  { logPackage :: !Text
  , logSuites :: ![SuiteRun]
  }
  deriving (Eq, Ord, Show)
