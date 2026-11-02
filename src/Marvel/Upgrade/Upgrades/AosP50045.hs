{-# OPTIONS_GHC -Wno-unused-imports #-}
module Marvel.Upgrade.Upgrades.AosP50045 where

import Marvel.Prelude
import Marvel.Question
import Marvel.Id
import Marvel.Projection
import Marvel.Card
import Marvel.Cost
import Marvel.Keyword
import Marvel.Criteria
import Marvel.Damage
import Marvel.Game.Source
import Marvel.Matchers
import Marvel.Payment
import Marvel.Query
import Marvel.Resource.Types qualified as R
import Marvel.Trait qualified as T
import Marvel.Window qualified as W
import Marvel.AgentsOfShield.PlayerHelpers

import Marvel.Ability
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Upgrade.Cards qualified as Cards
import Marvel.Upgrade.Types

aosP50045 :: UpgradeCard AosP50045
aosP50045 = upgrade AosP50045 Cards.aosP50045

newtype AosP50045 = AosP50045 (Attrs Upgrade)
  deriving anyclass (IsUpgrade, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities AosP50045 where
  getAbilities (AosP50045 attrs) =
    [limitedWindowAbility attrs 1 (W.EncounterCardRevealedBy W.When (IdentityWithId $ upgradeController attrs) (EncounterCardMatching $ CardWithType TreacheryType)) Interrupt (OwnsThis <> CanCancelRevealedEncounterCard) (DiscardCost (toRef attrs) <> CountersFromUpgradeCost (UpgradeWithKeyword SuitForm <> UpgradeControlledBy You) 1) (runAbility attrs 1)]

instance RunMessage AosP50045 where
  runMessage msg card@(AosP50045 attrs) = runQueueT $ case msg of
    RanAbility _ (isTarget attrs -> True) 1 windows _ -> do
      for_ [encounter | W.EncounterCardRevealed _ _ encounter <- windows] $ \encounter -> cancelEncounterCard encounter >> discardCard encounter
      pure card
    _ -> AosP50045 <$> runMessage msg attrs
