module Marvel.Treachery.Treacheries.SmVenomGoblin where

import Marvel.Prelude
import Marvel.Ability.Types
import Marvel.Game.Source
import Marvel.MainScheme.MainSchemes.SmVenomGoblin qualified as Goblin
import Marvel.Query
import Marvel.Card.Code
import Marvel.Entity
import Marvel.GameValue
import Marvel.Hp
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Stats
import Marvel.Treachery.Cards qualified as Cards
import Marvel.Treachery.Types

newtype SmVenomGoblinTreachery = SmVenomGoblinTreachery (Attrs Treachery)
  deriving anyclass (IsTreachery)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

smSpreadingPanic :: TreacheryCard SmVenomGoblinTreachery
smSpreadingPanic = treachery SmVenomGoblinTreachery Cards.smSpreadingPanic

instance RunMessage SmVenomGoblinTreachery where
  runMessage msg e@(SmVenomGoblinTreachery attrs) = runQueueT $ case msg of
    TreacheryMessage tid (RevealTreachery you) | tid == treacheryId attrs -> e <$ special you
    Boost (RevealedAsBoost target enemy) | isTarget attrs target -> do
      you <- getBoostedPlayer enemy
      special you
      pure e
    _ -> SmVenomGoblinTreachery <$> runMessage msg attrs
   where
    special you = getGliderSchemes >>= traverse_ (Goblin.resolveGoblinSpecial you)
