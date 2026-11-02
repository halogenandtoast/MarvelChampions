{-# OPTIONS_GHC -Wno-unused-imports -Wno-unused-matches #-}
module Marvel.Upgrade.Upgrades.Fw53010 where
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

import Marvel.FalconWinter.Helpers
fw53010 :: UpgradeCard Fw53010
fw53010 = upgrade Fw53010 Cards.fw53010

newtype Fw53010 = Fw53010 (Attrs Upgrade)
  deriving anyclass (IsUpgrade, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities Fw53010 where
  getAbilities a@(Fw53010 attrs) =
    [limitedWindowAbility a 1 (W.WindowOneOf [W.MakesBasicAttack W.When You, W.MakesBasicThwart W.When You, W.MakesBasicDefense W.When You AnyEnemy]) HeroInterrupt (OwnsThis <> EncounterTopHasEffect) (ExhaustCost <> DiscardTopEncounterCardCost) (runAbility a 1)]

instance RunMessage Fw53010 where
  runMessage msg a@(Fw53010 attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 windows payment -> do
      countFalconIcons attrs "awareness" you [card | EncounterCard card <- paymentEncounterDiscards payment]
      pure a
    WithBoostIcons (isTarget attrs -> True) value counts -> a <$ resolveFalconIcons attrs value counts
    CardSpecific (isTarget attrs -> True) "awareness" (fromJSON -> Success (you, bonus :: Natural)) -> do
      addToBasicPower you bonus
      pure a
    _ -> Fw53010 <$> runMessage msg attrs
