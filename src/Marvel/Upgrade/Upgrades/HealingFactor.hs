module Marvel.Upgrade.Upgrades.HealingFactor where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card.Code
import Marvel.Cost
import Marvel.Criteria
import Marvel.Entity
import Marvel.Game.Source ()
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query ()
import Marvel.Question (runAbility)
import Marvel.Ref
import Marvel.Upgrade.Cards qualified as Cards
import Marvel.Upgrade.Types
import Marvel.Phase (Phase (PlayerPhase))
import Marvel.Window qualified as W

healingFactor :: UpgradeCard HealingFactor
healingFactor = upgrade HealingFactor Cards.healingFactor

newtype HealingFactor = HealingFactor (Attrs Upgrade)
  deriving anyclass (IsUpgrade, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities HealingFactor where
  getAbilities a =
    [ limitedWindowAbility a 1 (W.PhaseBegins W.After PlayerPhase) Response
        (OwnsThis <> SelfMatches (IdentityWithAnyDamage <> IdentityCanBeHealed)) ExhaustCost (runAbility a 1)
    ]

instance RunMessage HealingFactor where
  runMessage msg u@(HealingFactor attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ _ -> u <$ heal you 2
    _ -> HealingFactor <$> runMessage msg attrs
