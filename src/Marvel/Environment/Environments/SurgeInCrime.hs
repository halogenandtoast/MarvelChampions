module Marvel.Environment.Environments.SurgeInCrime where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Entity
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Environment.Cards qualified as Cards
import Marvel.Environment.Types
import Marvel.Criteria
import Marvel.Question (runAbility)
import Marvel.Keyword qualified as Keyword
import Marvel.Query
import Marvel.Trait

surgeInCrime :: EnvironmentCard SurgeInCrime
surgeInCrime = environment SurgeInCrime Cards.surgeInCrime

newtype SurgeInCrime = SurgeInCrime (Attrs Environment)
  deriving anyclass (IsEnvironment)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities SurgeInCrime where
  getAbilities a =
    [ ability a 1 HeroAction (NotCriteria $ MinionExists $ MinionWithTrait Criminal)
        (MultiResourceCost [Nothing, Nothing]) (runAbility a 1)
    ]

instance HasModifiersFor SurgeInCrime where
  getModifiersFor _ (MinionRef mid) _ = do
    criminal <- minionMatches (MinionWithTrait Criminal) mid
    pure [KeywordModifier Keyword.Surge | criminal]
  getModifiersFor _ _ _ = pure []

instance RunMessage SurgeInCrime where
  runMessage msg e@(SurgeInCrime attrs) = runQueueT $ case msg of
    RanAbility _ (isTarget attrs -> True) 1 _ _ -> e <$ removeFromPlay attrs
    _ -> SurgeInCrime <$> runMessage msg attrs
