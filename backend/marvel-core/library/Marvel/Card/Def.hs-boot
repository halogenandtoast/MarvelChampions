module Marvel.Card.Def where

import Marvel.Prelude

data CardMatcher

instance Show CardMatcher
instance Eq CardMatcher
instance FromJSON CardMatcher
instance ToJSON CardMatcher
instance Hashable CardMatcher

data CardDef

instance Show CardDef
instance Eq CardDef
instance FromJSON CardDef
instance ToJSON CardDef
instance Hashable CardDef
