module Marvel.Upgrade.Upgrades.PsychicLink where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Cost
import Marvel.Criteria
import Marvel.Matchers
import Marvel.Question (runAbility)
import Marvel.Window qualified as W
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Upgrade.Cards qualified as Cards
import Marvel.Upgrade.Types

psychicLink :: UpgradeCard PsychicLink
psychicLink = upgrade PsychicLink Cards.psychicLink

newtype PsychicLink = PsychicLink (Attrs Upgrade)
  deriving anyclass (IsUpgrade, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities PsychicLink where
  getAbilities a =
    [ limitedWindowAbility a 1 (W.MakesBasicThwart W.When You) HeroInterrupt OwnsThis ExhaustCost (runAbility a 1)
    ]

instance RunMessage PsychicLink where
  runMessage msg u@(PsychicLink attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ _ -> u <$ addToBasicPower you 2
    _ -> PsychicLink <$> runMessage msg attrs
