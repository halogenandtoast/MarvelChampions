{-# OPTIONS_GHC -Wno-unused-imports -Wno-unused-matches #-}
module Marvel.Upgrade.Upgrades.Fw53009 where
import Marvel.Prelude
import Data.Maybe (maybeToList)
import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage
import Marvel.Entity
import Marvel.Game.Source
import Marvel.GameValue
import Marvel.Hand
import Marvel.Hp
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Payment
import Marvel.Projection
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.Resource qualified as R
import Marvel.Stats
import Marvel.Trait qualified as T
import Marvel.Window qualified as W
import Marvel.FalconWinter.Cards qualified as Cards
import Marvel.Upgrade.Types

fw53009 :: UpgradeCard Fw53009
fw53009 = upgrade Fw53009 Cards.fw53009

newtype Fw53009 = Fw53009 (Attrs Upgrade)
  deriving anyclass (IsUpgrade, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities Fw53009 where
  getAbilities a@(Fw53009 attrs) =
    [ability a 1 HeroAction OwnsThis (ExhaustCost <> DealEncounterCardToPlayerCost AnyIdentity) (runAbility a 1)]
    <> [limitedWindowAbility a 2 (W.EncounterCardWouldBeDealt AnyIdentity) Interrupt OwnsThis UseCost (runAbility a 2) | upgradeUses attrs > 0]

instance RunMessage Fw53009 where
  runMessage msg a@(Fw53009 attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 windows payment -> do
      placeCounters attrs 1
      pure a
    RanAbility _ (isTarget attrs -> True) 2 windows _ -> do
      for_ [you | W.EncounterCardBeingDealt you <- windows] cancelDealtEncounterCard
      pure a
    _ -> Fw53009 <$> runMessage msg attrs
