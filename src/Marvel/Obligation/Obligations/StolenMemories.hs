module Marvel.Obligation.Obligations.StolenMemories (
  stolenMemories,
  StolenMemories (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Card
import Marvel.Deck
import Marvel.Entity
import Marvel.Identity.Types (Field (PlayerIdentityDeck))
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Obligation.Cards qualified as Cards
import Marvel.Obligation.Types
import Marvel.Projection
import Marvel.Ref
import Marvel.Resource

stolenMemories :: ObligationCard StolenMemories
stolenMemories = obligation StolenMemories Cards.stolenMemories

newtype StolenMemories = StolenMemories (Attrs Obligation)
  deriving anyclass (IsObligation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Alter-Ego Action: discard a [mental] resource from your hand → discard this
-- obligation, and each facedown card under it.
instance HasAbilities StolenMemories where
  getAbilities (StolenMemories a) = discardToDiscardAbility a (withResource Mental)

-- | When Revealed: place the top 8 cards of your deck facedown under this card. It stays
-- in your play area.
instance RunMessage StolenMemories where
  runMessage msg o@(StolenMemories attrs) = runQueueT $ case msg of
    ObligationMessage ident (RevealObligation you) | ident == obligationId attrs -> do
      attrs' <- runMessage msg attrs
      cards <- take 8 . unDeck <$> project PlayerIdentityDeck you
      for_ cards (removeFromGame . CardIdRef . pcCardId)
      pure . StolenMemories $ attrs' & heldCardsL <>~ cards
    ObligationMessage ident (ResolvedObligation _) | ident == obligationId attrs -> pure o
    RanAbility you (isTarget attrs -> True) 1 _ _ -> o <$ discardToDiscard attrs you (withResource Mental)
    _ -> StolenMemories <$> runMessage msg attrs
