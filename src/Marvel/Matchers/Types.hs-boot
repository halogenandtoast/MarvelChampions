module Marvel.Matchers.Types where

import Marvel.Prelude

data UpgradeMatcher
data AllyMatcher
data SupportMatcher

instance Show SupportMatcher
instance Eq SupportMatcher
instance ToJSON SupportMatcher
instance FromJSON SupportMatcher
instance Hashable SupportMatcher

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

data EnemyMatcher

instance Show EnemyMatcher
instance Eq EnemyMatcher
instance ToJSON EnemyMatcher
instance FromJSON EnemyMatcher
instance Hashable EnemyMatcher

data CharacterMatcher

instance Show CharacterMatcher
instance Eq CharacterMatcher
instance ToJSON CharacterMatcher
instance FromJSON CharacterMatcher
instance Hashable CharacterMatcher
