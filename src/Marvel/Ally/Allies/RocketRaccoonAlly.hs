module Marvel.Ally.Allies.RocketRaccoonAlly (
  rocketRaccoonAlly,
  RocketRaccoonAlly (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Ally.Cards qualified as Cards
import Marvel.Ally.Runner
import Marvel.Cost
import Marvel.Criteria
import Marvel.Matchers hiding (ExhaustedAlly)
import Marvel.Window qualified as W

rocketRaccoonAlly :: AllyCard RocketRaccoonAlly
rocketRaccoonAlly = ally RocketRaccoonAlly Cards.rocketRaccoonAlly (Thw 2, 1) (Atk 1, 1) (HP 3)

newtype RocketRaccoonAlly = RocketRaccoonAlly (Attrs Ally)
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Interrupt: When Rocket Raccoon attacks a minion, he gets +3 ATK for that attack.
-- That attack gains overkill.
instance HasAbilities RocketRaccoonAlly where
  getAbilities a@(RocketRaccoonAlly attrs) =
    [ limitedWindowAbility
        a
        1
        (W.AllyAttacked W.When (AllyWithId $ allyId attrs) MinionEnemy)
        Interrupt
        OwnsThis
        NoCost
        (runAbility a 1)
    ]

instance RunMessage RocketRaccoonAlly where
  runMessage msg a@(RocketRaccoonAlly attrs) = runQueueT $ case msg of
    RanAbility _ (isTarget a -> True) 1 _ _ -> do
      raiseAllyAttack (allyId attrs) 3
      allyAttackGainsOverkill (allyId attrs)
      pure a
    _ -> RocketRaccoonAlly <$> runMessage msg attrs
