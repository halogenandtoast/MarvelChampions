module Marvel.Upgrade.Upgrades.StormsCrown where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card.Def
import Marvel.Cost
import Marvel.Criteria
import Marvel.Matchers
import Marvel.Question
import Marvel.Support.Cards qualified as Supports
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Upgrade.Cards qualified as Cards
import Marvel.Upgrade.Types

stormsCrown :: UpgradeCard StormsCrown
stormsCrown = upgrade StormsCrown Cards.stormsCrown

newtype StormsCrown = StormsCrown (Attrs Upgrade)
  deriving anyclass (IsUpgrade)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasModifiersFor StormsCrown where
  getModifiersFor _ (IdentityRef you) (StormsCrown attrs) | you == upgradeController attrs = pure [ThwartModifier 1]
  getModifiersFor _ _ _ = pure []

instance HasAbilities StormsCrown where
  getAbilities (StormsCrown attrs) =
    [ ability attrs n HeroResource (OwnsThis <> SupportExists (SupportMatches [SupportNamed $ cdName weather, SupportControlledBy $ IdentityWithId $ upgradeController attrs])) ExhaustCost (Pay $ ResourcePayment resource)
    | (n, weather) <- zip [1 ..] [Supports.clearSkies, Supports.hurricane, Supports.thunderstorm, Supports.blizzard]
    , resource <- take 1 (printedResources weather)
    ]

instance RunMessage StormsCrown where
  runMessage msg (StormsCrown attrs) = runQueueT $ StormsCrown <$> runMessage msg attrs
