module Marvel.Minion.Minions.SmMysterio where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Cost.Types
import Marvel.Game.Source
import Marvel.Matchers
import Marvel.Question ()
import Marvel.Window qualified as W
import Marvel.Card.Code
import Marvel.Entity
import Marvel.GameValue ()
import Marvel.Hp
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Stats
import Marvel.Minion.Cards qualified as Cards
import Marvel.Minion.Types
import Marvel.Minion.Runner

newtype SmMysterioMinion = SmMysterioMinion (Attrs Minion)
  deriving anyclass (IsMinion, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

smShiftingApparition :: MinionCard SmMysterioMinion
smShiftingApparition = minion SmMysterioMinion Cards.smShiftingApparition (Sch 1) (Atk 1) (HP 1)

instance HasAbilities SmMysterioMinion where
  getAbilities a@(SmMysterioMinion attrs) =
    [windowAbility a 1 (W.MinionDefeated W.When $ MinionWithId $ minionId attrs) ForcedInterrupt NoCost (runAbility a 1)]

instance RunMessage SmMysterioMinion where
  runMessage msg m@(SmMysterioMinion attrs) = runQueueT $ case msg of
    RanAbility _ (isTarget attrs -> True) 1 windows _ -> do
      for_ [source | W.DefeatedMinion mid _ source excess <- windows, mid == minionId attrs, excess > 0] \source -> do
        player <- getSourceController source
        for_ player (`shuffleTopEncounterCardsIntoPlayerDeck` 1)
      pure m
    _ -> SmMysterioMinion <$> runMessage msg attrs
