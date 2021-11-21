module Marvel.Name where

import Marvel.Prelude

data Name = Name Text (Maybe Text)
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)

(<:>) :: Text -> Text -> Name
(<:>) title = Name title . Just

instance IsString Name where
  fromString = flip Name Nothing . fromString
