module Marvel.Upgrade.Upgrades.PowerWithin where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Upgrade.Cards qualified as Cards
import Marvel.Upgrade.Types
import Marvel.Cost.Types
import Marvel.Criteria
import Marvel.Matchers
import Marvel.Question (runAbility)
import Marvel.Window qualified as W

powerWithin :: UpgradeCard PowerWithin
powerWithin = upgrade PowerWithin Cards.powerWithin

newtype PowerWithin = PowerWithin (Attrs Upgrade)
  deriving anyclass (IsUpgrade, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities PowerWithin where
  getAbilities a =
    [ limitedWindowAbility a 1
        (W.WindowOneOf [W.MakesBasicAttack W.After You, W.MakesBasicThwart W.After You, W.HeroDefended W.After You AnyEnemy])
        HeroResponse (OwnsThis <> InHeroForm <> CanResolvePrintedHeroAbilities) (DiscardCost $ toTarget a) (runAbility a 1) ]

instance RunMessage PowerWithin where
  runMessage msg u@(PowerWithin attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ _ -> do
      cardSpecific (IdentityRef you) "venomBlast" ()
      pure u
    _ -> PowerWithin <$> runMessage msg attrs
