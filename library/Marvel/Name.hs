module Marvel.Name where

import Marvel.Prelude

data Name = Name Text (Maybe Text)
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance IsString Name where
  fromString = flip Name Nothing . fromString
