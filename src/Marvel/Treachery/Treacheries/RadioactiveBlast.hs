module Marvel.Treachery.Treacheries.RadioactiveBlast (
  radioactiveBlast,
  RadioactiveBlast (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Damage
import Marvel.Entity
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Ref
import Marvel.Treachery.Cards qualified as Cards
import Marvel.Treachery.Types

radioactiveBlast :: TreacheryCard RadioactiveBlast
radioactiveBlast = treachery RadioactiveBlast Cards.radioactiveBlast

newtype RadioactiveBlast = RadioactiveBlast (Attrs Treachery)
  deriving anyclass (IsTreachery)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | When Revealed (Alter-Ego): place 2 threat on the main scheme.
-- When Revealed (Hero): take 2 damage.
instance RunMessage RadioactiveBlast where
  runMessage msg t@(RadioactiveBlast attrs) = runQueueT $ case msg of
    TreacheryMessage ident (RevealTreachery you) | ident == treacheryId attrs -> do
      isHero <- identityMatches HeroIdentity you
      if isHero
        then damageIdentity you attrs (toDamage 2 FromTreachery)
        else placeThreatOnEach attrs 2 MainScheme
      pure t
    _ -> RadioactiveBlast <$> runMessage msg attrs
