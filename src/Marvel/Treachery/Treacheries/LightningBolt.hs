module Marvel.Treachery.Treacheries.LightningBolt (
  lightningBolt,
  LightningBolt (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Card.Def ()
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Ref
import Marvel.Treachery.Cards qualified as Cards
import Marvel.Treachery.Types

lightningBolt :: TreacheryCard LightningBolt
lightningBolt = treachery LightningBolt Cards.lightningBolt

newtype LightningBolt = LightningBolt (Attrs Treachery)
  deriving anyclass (IsTreachery)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Discard 2 cards from the encounter deck: 1 indirect damage for each boost icon on
-- them.
instance RunMessage LightningBolt where
  runMessage msg t@(LightningBolt attrs) = runQueueT $ case msg of
    TreacheryMessage ident (RevealTreachery you) | ident == treacheryId attrs -> do
      discardTopOfEncounterDeckThen 2 (countBoostIcons attrs you)
      pure t
    WithBoostIcons (isTarget attrs -> True) (fromJSON -> Success you) (sum . map snd -> n) -> do
      when (n > 0) $ dealIndirectDamage you attrs n
      pure t
    -- [star] Boost: discard 3 cards from the encounter deck
    Boost (RevealedAsBoost target _) | isTarget attrs target -> do
      discardTopOfEncounterDeck 3
      pure t
    _ -> LightningBolt <$> runMessage msg attrs
