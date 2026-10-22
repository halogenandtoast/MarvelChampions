module Marvel.Treachery.Treacheries.SniperShot (
  sniperShot,
  SniperShot (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Damage
import Marvel.Entity
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Query
import Marvel.Treachery.Cards qualified as Cards
import Marvel.Treachery.Types

sniperShot :: TreacheryCard SniperShot
sniperShot = treachery SniperShot Cards.sniperShot

newtype SniperShot = SniperShot (Attrs Treachery)
  deriving anyclass (IsTreachery)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

-- | When Revealed (Alter-Ego): Place 3 threat on the main scheme. When Revealed
-- (Hero): Deal 3 damage to your hero.
instance RunMessage SniperShot where
  runMessage msg t@(SniperShot attrs) = runQueueT $ case msg of
    TreacheryMessage ident (RevealTreachery identityId) | treacheryId attrs == ident -> do
      isHero <- identityMatches HeroIdentity identityId
      if isHero
        then damageIdentity identityId attrs (toDamage 3 FromTreachery)
        else do
          mainScheme <- selectJust MainScheme
          placeThreat mainScheme 3
      pure t
    _ -> SniperShot <$> runMessage msg attrs
