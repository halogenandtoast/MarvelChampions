module Marvel.Treachery.Treacheries.ElectromagneticPulse (
  electromagneticPulse,
  ElectromagneticPulse (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Card.Def
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Minion.Cards qualified as Minions
import Marvel.Ref
import Marvel.Treachery.Cards qualified as Cards
import Marvel.Treachery.Types

electromagneticPulse :: TreacheryCard ElectromagneticPulse
electromagneticPulse = treachery ElectromagneticPulse Cards.electromagneticPulse

newtype ElectromagneticPulse = ElectromagneticPulse (Attrs Treachery)
  deriving anyclass (IsTreachery)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Discard 7 cards from the encounter deck: Electro among them is put into play engaged
-- with you, otherwise this card gains surge.
instance RunMessage ElectromagneticPulse where
  runMessage msg t@(ElectromagneticPulse attrs) = runQueueT $ case msg of
    TreacheryMessage ident (RevealTreachery you) | ident == treacheryId attrs -> do
      discardTopOfEncounterDeckThen 7 \cards ->
        case filter (cardMatch (CardIs Minions.electro)) cards of
          electro : _ -> putEncounterCardIntoPlay you electro
          [] -> gainSurge attrs
      pure t
    GainSurge target | isTarget attrs target -> pure . ElectromagneticPulse $ attrs & surgeL .~ True
    -- [star] Boost: discard 3 cards from the encounter deck
    Boost (RevealedAsBoost target _) | isTarget attrs target -> do
      discardTopOfEncounterDeck 3
      pure t
    _ -> ElectromagneticPulse <$> runMessage msg attrs
