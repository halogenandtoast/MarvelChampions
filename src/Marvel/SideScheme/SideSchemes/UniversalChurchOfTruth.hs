module Marvel.SideScheme.SideSchemes.UniversalChurchOfTruth (
  universalChurchOfTruth,
  UniversalChurchOfTruth (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Entity
import Marvel.Game.Source
import Marvel.GameValue
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Ref
import Marvel.SideScheme.Cards qualified as Cards
import Marvel.SideScheme.Types
import Marvel.Window.Types qualified as W

-- | 2 threat per player, an acceleration icon.
universalChurchOfTruth :: SideSchemeCard UniversalChurchOfTruth
universalChurchOfTruth = sideScheme UniversalChurchOfTruth Cards.universalChurchOfTruth (PerPlayer 2)

newtype UniversalChurchOfTruth = UniversalChurchOfTruth (Attrs SideScheme)
  deriving anyclass (IsSideScheme, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Forced Response: After a player resets their deck, exhaust that player identity and
-- stun it. [star] Boost: Reveal this card.
instance RunMessage UniversalChurchOfTruth where
  runMessage msg s@(UniversalChurchOfTruth attrs) = runQueueT $ case msg of
    CheckWindows ws | resets@(_ : _) <- [p | W.Window W.After (W.PlayerDeckReset p) <- ws] -> do
      -- in play, not just defeated
      inPlay <- inAnyArea $ selectAny (SideSchemeWithId $ sideSchemeId attrs)
      when inPlay $ for_ resets \p -> do
        exhaust p
        stunIdentity p
      pure s
    Boost (RevealedAsBoost target enemy) | isTarget attrs target -> do
      revealBoost attrs =<< getBoostedPlayer enemy
      pure s
    _ -> UniversalChurchOfTruth <$> runMessage msg attrs
