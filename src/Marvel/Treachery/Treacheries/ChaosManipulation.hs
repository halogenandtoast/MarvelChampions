module Marvel.Treachery.Treacheries.ChaosManipulation (
  chaosManipulation,
  ChaosManipulation (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Card.Def
import Marvel.Entity
import Marvel.Id (IdentityId)
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Minion.Cards qualified as Minions
import Marvel.Query
import Marvel.Ref
import Marvel.Treachery.Cards qualified as Cards
import Marvel.Treachery.Types

chaosManipulation :: TreacheryCard ChaosManipulation
chaosManipulation = treachery ChaosManipulation Cards.chaosManipulation

newtype ChaosManipulation = ChaosManipulation (Attrs Treachery)
  deriving anyclass (IsTreachery)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Search the encounter deck and discard pile for Luminous and put her into play engaged
-- with you. Discard the top card of the encounter deck: 2 or more boost icons on it,
-- Luminous activates against you (attacks your hero, schemes against your alter-ego),
-- whether found now or already in play.
instance RunMessage ChaosManipulation where
  runMessage msg t@(ChaosManipulation attrs) = runQueueT $ case msg of
    TreacheryMessage ident (RevealTreachery you) | ident == treacheryId attrs -> do
      -- in play, she is not found; the deck is shuffled all the same
      searchAndPutIntoPlayEncounterCard you (CardIs Minions.luminous)
      -- the deck as the search left it
      cardSpecific attrs "discard" you
      pure t
    CardSpecific (isTarget attrs -> True) "discard" (fromJSON -> Success you) -> do
      discardTopOfEncounterDeckThen 1 (countBoostIcons attrs (you :: IdentityId))
      pure t
    WithBoostIcons (isTarget attrs -> True) (fromJSON -> Success you) (sum . map snd -> n) | n >= 2 -> do
      selectOne (MinionIs Minions.luminous) >>= traverse_ \luminous -> do
        isHero <- identityMatches HeroIdentity you
        if isHero then enemyAttacks luminous you else minionSchemes luminous
      pure t
    _ -> ChaosManipulation <$> runMessage msg attrs
