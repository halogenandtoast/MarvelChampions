module Marvel.Treachery.Treacheries.PlanetaryInvasion (
  planetaryInvasion,
  PlanetaryInvasion (..),
) where

import Marvel.Prelude

import Marvel.Card
import Marvel.Entity
import Marvel.Id
import Marvel.Message.Lifted
import Marvel.Ref
import Marvel.Treachery.Cards qualified as Cards
import Marvel.Treachery.Types

planetaryInvasion :: TreacheryCard PlanetaryInvasion
planetaryInvasion = treachery PlanetaryInvasion Cards.planetaryInvasion

newtype PlanetaryInvasion = PlanetaryInvasion (Attrs Treachery)
  deriving anyclass (IsTreachery)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | When Revealed: Discard cards from the top of the encounter deck until you discard a
-- minion. Reveal that minion, then give it a tough status card.
instance RunMessage PlanetaryInvasion where
  runMessage msg t@(PlanetaryInvasion attrs) = runQueueT $ case msg of
    TreacheryMessage ident (RevealTreachery you) | ident == treacheryId attrs -> do
      discardUntil FromEncounterDeck (CardWithType MinionType) attrs
      pure . PlanetaryInvasion $ attrs & resolverL ?~ you
    WithDiscardedMatch target FromEncounterDeck (EncounterCard minion) | isTarget attrs target -> do
      for_ (treacheryResolver attrs) \you -> do
        revealEncounterCard you minion
        -- the reveal resolves first; a minion gone by then takes no status card
        enemyBecomesTough (MinionId $ unCardId $ ecCardId minion)
      pure t
    _ -> PlanetaryInvasion <$> runMessage msg attrs
