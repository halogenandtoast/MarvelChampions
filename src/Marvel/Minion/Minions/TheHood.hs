module Marvel.Minion.Minions.TheHood where

import Marvel.Ability
import Marvel.Card
import Marvel.Game.Source
import Marvel.Minion.Cards qualified as Cards
import Marvel.Minion.Runner
import Marvel.Prelude
import Marvel.Villain.Villains.TheHood

newtype MadameMasque = MadameMasque (Attrs Minion)
  deriving anyclass (IsMinion, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

madameMasque :: MinionCard MadameMasque
madameMasque = minion MadameMasque Cards.madameMasque (Sch 1) (Atk 2) (HP 4)

instance RunMessage MadameMasque where
  runMessage msg e@(MadameMasque attrs) = runQueueT $ case msg of
    MinionMessage mid (RevealMinion you) | mid == minionId attrs -> e <$ foulPlay you
    MinionMessage mid (MinionDefeatedWithExcess source _ _) | mid == minionId attrs -> do
      you <- maybe getActivePlayerId pure =<< getSourceController source
      foulPlay you
      MadameMasque <$> runMessage msg attrs
    _ -> MadameMasque <$> runMessage msg attrs
