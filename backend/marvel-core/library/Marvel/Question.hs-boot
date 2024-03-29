module Marvel.Question where

import Marvel.Prelude

data Question

instance Show Question
instance Eq Question
instance ToJSON Question
instance FromJSON Question

data Choice

instance Show Choice
instance Eq Choice
instance ToJSON Choice
instance FromJSON Choice
