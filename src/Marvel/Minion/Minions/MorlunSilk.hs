module Marvel.Minion.Minions.MorlunSilk where
import Marvel.Prelude
import Marvel.Cost ()
import Marvel.Criteria ()
import Marvel.Matchers ()
import Marvel.Question ()
import Marvel.Window qualified as W ()
import Marvel.Trait qualified as T ()
import Marvel.Damage ()
import Marvel.Game.Source
import Marvel.Card

import Marvel.Ability
import Marvel.Card.Code ()
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Silk.Cards qualified as Cards
import Marvel.Minion.Runner

morlunSilk :: MinionCard MorlunSilk
morlunSilk = minion MorlunSilk Cards.morlunSilk (Sch 1) (Atk 1) (HP 5)
newtype MorlunSilk = MorlunSilk (Attrs Minion)
  deriving anyclass (IsMinion, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)
instance RunMessage MorlunSilk where
  runMessage msg u@(MorlunSilk attrs) = runQueueT $ case msg of
    MinionMessage mid MinionDefeated | mid == minionId attrs -> do
      players <- getPlayers
      for_ players $ \you -> do
        cards <- tuckedEncounterCards you
        traverse_ (discardTuckedEncounterCardFromEncounter you) [card | card <- cards, toCardCode card == "52031"]
      pure u
    _ -> MorlunSilk <$> runMessage msg attrs

instance HasModifiersFor MorlunSilk where
  getModifiersFor _ (MinionRef mid) (MorlunSilk attrs) | mid == minionId attrs = do
    players <- getPlayers
    n <- sum <$> traverse (fmap (fromIntegral . length) . tuckedEncounterCards) players
    pure [AttackModifier n, SchemeModifier n]
  getModifiersFor _ _ _ = pure []
