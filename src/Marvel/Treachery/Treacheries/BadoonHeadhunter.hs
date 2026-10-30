-- | The Badoon Headhunter modular set's treacheries (The Galaxy's Most Wanted), each with
-- surge and "[star] Boost: Give the villain 1 additional boost card for this activation".
module Marvel.Treachery.Treacheries.BadoonHeadhunter (
  onTheHunt,
  OnTheHunt (..),
  deadToRights,
  DeadToRights (..),
  anotherBoostCard,
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Damage
import Marvel.Entity
import Marvel.Hand
import Marvel.Id (EnemyId (..))
import Marvel.Identity.Types (Field (PlayerIdentityHand))
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Projection
import Marvel.Query
import Marvel.Ref
import Marvel.Treachery.Cards qualified as Cards
import Marvel.Treachery.Types

-- | [star] Boost: another boost card for the villain's activation.
anotherBoostCard :: (ReverseQueue m) => Attrs Treachery -> Message -> Maybe (m ())
anotherBoostCard attrs = \case
  Boost (RevealedAsBoost target enemy) | isTarget attrs target -> Just case enemy of
    EnemyVillainId villain -> dealBoost villain
    -- turned up for a villainous minion's, it still gives "the villain" its card (RR
    -- Villainous), facedown on him for his next activation
    EnemyMinionId _ -> selectOne ActiveVillain >>= traverse_ dealBoost
  _ -> Nothing

onTheHunt :: TreacheryCard OnTheHunt
onTheHunt = treachery OnTheHunt Cards.onTheHunt

-- | When Revealed: choose to either take 2 damage or discard 1 card at random from your
-- hand (an empty hand leaves only the damage).
newtype OnTheHunt = OnTheHunt (Attrs Treachery)
  deriving anyclass (IsTreachery)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage OnTheHunt where
  runMessage msg t@(OnTheHunt attrs) = runQueueT $ case msg of
    TreacheryMessage ident (RevealTreachery you) | ident == treacheryId attrs -> do
      cards <- projectP PlayerIdentityHand (notNull . unHand) you
      chooseOrRunOne you $
        labeled (countLabel "takeDamage" 2) (damageIdentity you attrs (toDamage 2 FromTreachery))
          : [labeled (countLabel "discardRandomCardsFromHand" 1) (discardFrom you RandomFromHand 1) | cards]
      pure t
    _ | Just boost <- anotherBoostCard attrs msg -> t <$ boost
    _ -> OnTheHunt <$> runMessage msg attrs

deadToRights :: TreacheryCard DeadToRights
deadToRights = treachery DeadToRights Cards.deadToRights

-- | When Revealed: exhaust your identity; if you cannot, place 2 threat on the main
-- scheme.
newtype DeadToRights = DeadToRights (Attrs Treachery)
  deriving anyclass (IsTreachery)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage DeadToRights where
  runMessage msg t@(DeadToRights attrs) = runQueueT $ case msg of
    TreacheryMessage ident (RevealTreachery you) | ident == treacheryId attrs -> do
      ready <- identityMatches UnexhaustedIdentity you
      if ready
        then exhaust you
        else selectList EncounterMainScheme >>= traverse_ (`placeThreat` 2)
      pure t
    _ | Just boost <- anotherBoostCard attrs msg -> t <$ boost
    _ -> DeadToRights <$> runMessage msg attrs
