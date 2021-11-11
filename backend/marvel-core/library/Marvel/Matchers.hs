module Marvel.Matchers where

data IdentityMatcher = HeroIdentity | UnexhaustedIdentity
data AllyMatcher = UnexhaustedAlly
data EnemyMatcher = AnyEnemy
data VillainMatcher = ActiveVillain
data SchemeMatcher = AnyScheme | MainScheme
