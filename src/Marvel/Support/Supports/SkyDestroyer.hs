module Marvel.Support.Supports.SkyDestroyer where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Support.Cards qualified as Cards
import Marvel.Support.Types
import Marvel.Card
import Marvel.Cost.Types
import Marvel.Criteria
import Marvel.Damage
import Marvel.Matchers
import Marvel.Question (runAbility)
import Marvel.Trait (Trait (Shield))
import Marvel.Window qualified as W

skyDestroyer :: SupportCard SkyDestroyer
skyDestroyer = support SkyDestroyer Cards.skyDestroyer

newtype SkyDestroyer = SkyDestroyer (Attrs Support)
  deriving anyclass (IsSupport, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities SkyDestroyer where
  getAbilities a = [limitedWindowAbility a 1 (W.PlaysCard W.After You (CardWithTrait Shield)) Response (OwnsThis <> EnemyExists DamageableEnemy) ExhaustCost (runAbility a 1)]

instance RunMessage SkyDestroyer where
  runMessage msg s@(SkyDestroyer attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ _ -> do
      chooseDamage you attrs (toDamage 2 FromAbility) DamageableEnemy
      pure s
    _ -> SkyDestroyer <$> runMessage msg attrs
