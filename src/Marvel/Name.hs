module Marvel.Name where

import Marvel.Prelude

data Name = Name
  { title :: Text
  , subtitle :: Maybe Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)

(<:>) :: Text -> Text -> Name
(<:>) t = Name t . Just

instance IsString Name where
  fromString = flip Name Nothing . fromString

-- | The name as printed: the title, then the subtitle after a colon.
display :: Name -> Text
display (Name t Nothing) = t
display (Name t (Just s)) = t <> ": " <> s
