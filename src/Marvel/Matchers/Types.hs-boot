module Marvel.Matchers.Types where

import Marvel.Prelude

data UpgradeMatcher
data AllyMatcher

instance Show UpgradeMatcher
instance Eq UpgradeMatcher
instance ToJSON UpgradeMatcher
instance FromJSON UpgradeMatcher
instance Hashable UpgradeMatcher

instance Show AllyMatcher
instance Eq AllyMatcher
instance ToJSON AllyMatcher
instance FromJSON AllyMatcher
instance Hashable AllyMatcher

data CharacterMatcher

instance Show CharacterMatcher
instance Eq CharacterMatcher
instance ToJSON CharacterMatcher
instance FromJSON CharacterMatcher
instance Hashable CharacterMatcher
