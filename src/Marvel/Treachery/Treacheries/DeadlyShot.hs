module Marvel.Treachery.Treacheries.DeadlyShot (
  deadlyShot,
  DeadlyShot (..),
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

deadlyShot :: TreacheryCard DeadlyShot
deadlyShot = treachery DeadlyShot Cards.deadlyShot

newtype DeadlyShot = DeadlyShot (Attrs Treachery)
  deriving anyclass (IsTreachery)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

-- | Discard an upgrade you control, then: as an alter-ego place 1 threat on the main
-- scheme, as a hero take 1 damage.
instance RunMessage DeadlyShot where
  runMessage msg t@(DeadlyShot attrs) = runQueueT $ case msg of
    TreacheryMessage ident (RevealTreachery identityId) | ident == treacheryId attrs -> do
      isHero <- identityMatches HeroIdentity identityId
      upgrades <- selectRefs (UpgradeControlledBy $ IdentityWithId identityId)
      mainSchemes <- selectList EncounterMainScheme
      chooseOne identityId [targeting u (removeFromPlay u) | u <- upgrades]
      if isHero
        then damageCharacter identityId attrs (toDamage 1 FromTreachery)
        else for_ mainSchemes (`placeThreat` 1)
      pure t
    _ -> DeadlyShot <$> runMessage msg attrs
