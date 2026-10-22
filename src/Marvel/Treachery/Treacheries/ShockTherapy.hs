module Marvel.Treachery.Treacheries.ShockTherapy (
  shockTherapy,
  ShockTherapy (..),
) where

import Marvel.Prelude

import Marvel.Boost (boostCount)
import Marvel.Card.Code
import Marvel.Card.Def
import Marvel.Entity
import Marvel.Game.Source
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Query
import Marvel.Ref
import Marvel.Treachery.Cards qualified as Cards
import Marvel.Treachery.Types

shockTherapy :: TreacheryCard ShockTherapy
shockTherapy = treachery ShockTherapy Cards.shockTherapy

newtype ShockTherapy = ShockTherapy (Attrs Treachery)
  deriving anyclass (IsTreachery)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Discard 1 card per player from the encounter deck: the villain heals 1 for each boost
-- icon on them.
instance RunMessage ShockTherapy where
  runMessage msg t@(ShockTherapy attrs) = runQueueT $ case msg of
    TreacheryMessage ident (RevealTreachery _) | ident == treacheryId attrs -> do
      players <- getPlayerCount
      -- with no villain in play (as between one Kang and the next), nothing
      selectOne ActiveVillain >>= \case
        Nothing -> pure t
        Just villain -> do
          discardTopOfEncounterDeckThen (fromIntegral players) \cards -> do
            let n = sum $ map (boostCount . cdBoostIcons . getCardDef) cards
            when (n > 0) $ heal villain n
          pure t
    -- [star] Boost: discard 3 cards from the encounter deck
    Boost (RevealedAsBoost target _) | isTarget attrs target -> do
      discardTopOfEncounterDeck 3
      pure t
    _ -> ShockTherapy <$> runMessage msg attrs
