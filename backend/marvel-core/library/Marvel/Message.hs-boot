module Marvel.Message where

import Marvel.Prelude

data Message
class RunMessage a
instance ToJSON Message
instance FromJSON Message
instance Show Message
instance Eq Message
