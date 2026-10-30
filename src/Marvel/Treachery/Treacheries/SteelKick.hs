module Marvel.Treachery.Treacheries.SteelKick (
  steelKick,
  SteelKick (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Entity
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Query
import Marvel.Trait
import Marvel.Ref (IsRef)
import Marvel.Treachery.Cards qualified as Cards
import Marvel.Treachery.Types

steelKick :: TreacheryCard SteelKick
steelKick = treachery SteelKick Cards.steelKick

newtype SteelKick = SteelKick (Attrs Treachery)
  deriving anyclass (IsTreachery)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Alter-ego: 2 threat on the main scheme; hero: 3 indirect damage. One more either way
-- if Absorbing Man has the Metal trait.
instance RunMessage SteelKick where
  runMessage msg t@(SteelKick attrs) = runQueueT $ case msg of
    TreacheryMessage ident (RevealTreachery you) | ident == treacheryId attrs -> do
      hero <- identityMatches HeroIdentity you
      metal <- selectAny $ VillainWithTrait Metal
      let extra = if metal then 1 else 0
      if hero
        then dealIndirectDamage you attrs (3 + extra)
        else placeThreatOnEach attrs (2 + extra) EncounterMainScheme
      pure t
    _ -> SteelKick <$> runMessage msg attrs
