module Marvel.Treachery.Treacheries.DeadlyShot (
  deadlyShot,
  DeadlyShot (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Choice
import Marvel.Damage
import Marvel.Entity
import Marvel.Matchers
import Marvel.Message
import Marvel.Query
import Marvel.Question
import Marvel.Queue
import Marvel.Ref
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
  runMessage msg t@(DeadlyShot attrs) = case msg of
    TreacheryMessage ident (RevealTreachery identityId) | ident == treacheryId attrs -> do
      isHero <- identityMatches HeroIdentity identityId
      upgrades <- selectRefs (UpgradeControlledBy $ IdentityWithId identityId)
      mainScheme <- selectJust MainScheme
      let
        discard =
          [Ask identityId $ ChooseOne [TargetLabel u [DiscardTarget u] | u <- upgrades] | notNull upgrades]
      pushAll $
        discard
          <> if isHero
            then damageIdentity identityId (toSource attrs) (toDamage 1 FromTreachery)
            else placeThreatOn mainScheme 1
      pure t
    _ -> DeadlyShot <$> runMessage msg attrs
