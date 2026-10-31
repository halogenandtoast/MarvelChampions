module Marvel.Minion.Minions.Spot where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Entity
import Marvel.Game.Source
import Marvel.Message.Lifted
import Marvel.Minion.Cards qualified as Cards
import Marvel.Minion.Runner

spot :: MinionCard Spot
spot = minion (Spot . (`With` Meta 0)) Cards.spot (Sch 1) (Atk 1) (HP 4)

newtype Meta = Meta {excessDamage :: Natural}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype Spot = Spot (Attrs Minion `With` Meta)
  deriving anyclass (HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance IsMinion Spot where
  toMinionAttrs (Spot (attrs `With` _)) = attrs
  overMinionAttrs f (Spot (attrs `With` meta)) = Spot (f attrs `With` meta)

instance RunMessage Spot where
  runMessage msg e@(Spot (attrs `With` meta)) = runQueueT $ case msg of
    MinionMessage mid (MinionDefeatedWithExcess _ _ excess) | mid == minionId attrs ->
      Spot . (`With` Meta excess) <$> runMessage msg attrs
    MinionMessage mid MinionDefeated | mid == minionId attrs, excessDamage meta == 0 -> do
      let you = minionEngagedIdentity attrs
      chooseOnePromptedAbout you "$label.resolveWhenDefeated" attrs
        [targeting attrs $ cardSpecific attrs "shuffleDefeated" ()]
      pure e
    CardSpecific (isTarget attrs -> True) "shuffleDefeated" _ -> do
      minionLeavingPlayStep (minionId attrs) "shuffleAfterLeaveWindow"
      pure e
    CardSpecific (isTarget attrs -> True) "shuffleAfterLeaveWindow" _ -> do
      for_ (minionUpgrades attrs) removeFromPlay
      for_ (minionAttachments attrs) defeatedLeavesPlay
      removeMinionFromGameWithoutLeaveWindow (minionId attrs)
      minionDisengages (minionEngagedIdentity attrs) (minionId attrs)
      shuffleIntoEncounterDeck [MkEncounterCard (CardId $ unMinionId $ minionId attrs) (getCardDef attrs)]
      pure e
    Boost (RevealedAsBoost target enemy) | isTarget attrs target -> do
      you <- getBoostedPlayer enemy
      putBoostIntoPlay target you
      pure e
    _ -> Spot . (`With` meta) <$> runMessage msg attrs
