module Marvel.Minion.Minions.Loki (
  loki,
  Loki (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Matchers
import Marvel.Minion.Cards qualified as Cards
import Marvel.Minion.Runner
import Marvel.Window qualified as W

loki :: MinionCard Loki
loki = minion Loki Cards.loki (Sch 2) (Atk 2) (HP 4)

newtype Loki = Loki (Attrs Minion)
  deriving anyclass (IsMinion, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities Loki where
  getAbilities (Loki a) =
    [ windowAbility a 1 (W.MinionDefeated W.When $ MinionWithId $ minionId a) ForcedInterrupt NoCost $
        RunAbility (toTarget a) 1
    ]

-- | When he would be defeated, the top card of the encounter deck is discarded: a
-- treachery heals all his damage instead.
instance RunMessage Loki where
  runMessage msg l@(Loki attrs) = runQueueT $ case msg of
    RanAbility _ (isTarget attrs -> True) 1 _ _ -> do
      discardTopOfEncounterDeckFor 1 attrs
      pure l
    WithDiscarded (isTarget attrs -> True) _ (onlyEncounterCards -> cards)
      | any ((== TreacheryType) . cdCardType . getCardDef) cards -> do
          cancelMinionDefeat (minionId attrs)
          healAllDamage (minionId attrs)
          pure l
    _ -> Loki <$> runMessage msg attrs
