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
