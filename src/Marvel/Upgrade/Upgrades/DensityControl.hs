module Marvel.Upgrade.Upgrades.DensityControl where

import Marvel.Prelude
import Marvel.Question
import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Entity
import Marvel.Query
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Upgrade.Cards qualified as Cards
import Marvel.Upgrade.Types
import Marvel.Event.Cards qualified as Events
import Marvel.Window qualified as W

densityControl :: UpgradeCard DensityControl
densityControl = upgrade DensityControl Cards.densityControl

newtype DensityControl = DensityControl (Attrs Upgrade)
  deriving anyclass (IsUpgrade, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

visionEvent :: CardMatcher
visionEvent = CardWithType EventType <> CardMatchesAny (map CardIs [Events.solarBeam, Events.superdenseStrike, Events.justPassingThrough, Events.phaseDisruption, Events.massIncrease])

instance HasAbilities DensityControl where
  getAbilities (DensityControl a) =
    [ limitedWindowAbility a 1 (W.MassFormChangedTo W.After $ UpgradeControlledBy $ IdentityWithId $ upgradeController a)
        HeroResponse (OwnsThis <> ExtendedCardExists (InDiscardOf (IdentityWithId $ upgradeController a) $ BasicCardMatches visionEvent))
        (DiscardCost $ toTarget a) (runAbility a 1)
    ]

instance RunMessage DensityControl where
  runMessage msg u@(DensityControl attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ _ -> do
      cards <- selectList (InDiscardOf (IdentityWithId you) $ BasicCardMatches visionEvent)
      unless (null cards) $ chooseOne you [targeting card (addToHand you card) | card <- cards]
      pure u
    _ -> DensityControl <$> runMessage msg attrs
