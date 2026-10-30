-- | The Loki set's treacheries (The Mad Titan's Shadow, scenario 5): "Loki" is the
-- villain.
module Marvel.Treachery.Treacheries.Loki (
  deviousSorcery,
  DeviousSorcery (..),
  infiniteMischief,
  InfiniteMischief (..),
  theTrickster,
  TheTrickster (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Damage
import Marvel.Entity
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Query
import Marvel.Ref
import Marvel.Treachery.Cards qualified as Cards
import Marvel.Treachery.Treacheries.BadoonHeadhunter (anotherBoostCard)
import Marvel.Treachery.Types

deviousSorcery :: TreacheryCard DeviousSorcery
deviousSorcery = treachery DeviousSorcery Cards.deviousSorcery

newtype DeviousSorcery = DeviousSorcery (Attrs Treachery)
  deriving anyclass (IsTreachery)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | When Revealed (Alter-Ego): you are stunned; if you were already stunned, place 2
-- threat on the main scheme. When Revealed (Hero): you are stunned; if you were already
-- stunned, take 2 damage.
instance RunMessage DeviousSorcery where
  runMessage msg t@(DeviousSorcery attrs) = runQueueT $ case msg of
    TreacheryMessage ident (RevealTreachery you) | ident == treacheryId attrs -> do
      stunned <- identityMatches StunnedIdentity you
      hero <- identityMatches HeroIdentity you
      if
        | not stunned -> stunIdentity you
        | hero -> damageIdentity you attrs (toDamage 2 FromTreachery)
        | otherwise -> placeThreatOnEach attrs 2 EncounterMainScheme
      pure t
    _ -> DeviousSorcery <$> runMessage msg attrs

infiniteMischief :: TreacheryCard InfiniteMischief
infiniteMischief = treachery InfiniteMischief Cards.infiniteMischief

newtype InfiniteMischief = InfiniteMischief (Attrs Treachery)
  deriving anyclass (IsTreachery)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | When Revealed: shuffle the Infinity Stone discard pile into the Infinity Stone deck
-- and reveal its top card. [star] Boost: discard the top card of the Infinity Stone deck;
-- its boost icons count for this activation as a boost card's would.
instance RunMessage InfiniteMischief where
  runMessage msg t@(InfiniteMischief attrs) = runQueueT $ case msg of
    TreacheryMessage ident (RevealTreachery you) | ident == treacheryId attrs -> do
      shuffleStoneDiscardIntoDeck
      revealTopInfinityStone you
      pure t
    Boost (RevealedAsBoost target enemy) | isTarget attrs target -> do
      discardTopInfinityStoneThen 1 (countActivationBoostIcons (enemyRef enemy))
      pure t
    _ -> InfiniteMischief <$> runMessage msg attrs

theTrickster :: TreacheryCard TheTrickster
theTrickster = treachery TheTrickster Cards.theTrickster

newtype TheTrickster = TheTrickster (Attrs Treachery)
  deriving anyclass (IsTreachery)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | When Revealed: swap Loki with a random set-aside Loki villain; Loki activates against
-- you. [star] Boost: give Loki an additional boost card and a tough status card.
instance RunMessage TheTrickster where
  runMessage msg t@(TheTrickster attrs) = runQueueT $ case msg of
    TreacheryMessage ident (RevealTreachery you) | ident == treacheryId attrs -> do
      selectOne ActiveVillain >>= traverse_ \v -> do
        swapVillain v
        villainActivatesAgainst v you
      pure t
    _ | Just boost <- anotherBoostCard attrs msg -> do
      boost
      traverse_ enemyBecomesTough =<< selectOne ActiveVillain
      pure t
    _ -> TheTrickster <$> runMessage msg attrs
