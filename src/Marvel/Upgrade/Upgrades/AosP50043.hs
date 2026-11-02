{-# OPTIONS_GHC -Wno-unused-imports #-}
module Marvel.Upgrade.Upgrades.AosP50043 where

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

aosP50043 :: UpgradeCard AosP50043
aosP50043 = upgrade AosP50043 Cards.aosP50043

newtype AosP50043 = AosP50043 (Attrs Upgrade)
  deriving anyclass (IsUpgrade, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities AosP50043 where
  getAbilities (AosP50043 attrs) =
    [limitedWindowAbility attrs 1 (W.ThreatWouldBePlaced W.AnyThreatSource MainScheme) HeroInterrupt OwnsThis (DiscardCost $ toRef attrs) (runAbility attrs 1)]

instance RunMessage AosP50043 where
  runMessage msg card@(AosP50043 attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 windows _ -> do
      for_ [(mid,n) | W.ThreatPlaced _ (SchemeMainSchemeId mid) n <- windows] $ \(mid,n) -> do
        let cap = min 3 n
        chooseOne you [labeled (countLabel "aosMoveThreat" amount) do
          insteadOf (\case MainSchemeMessage who (MainSchemePlaceThreat _) -> who == mid; _ -> False) (placeThreatWithoutWindow mid (subtractNatural amount n))
          placeSuitThreat you amount | amount <- [0..cap]]
      pure card
    _ -> AosP50043 <$> runMessage msg attrs
