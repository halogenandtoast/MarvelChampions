module Marvel.SideScheme.SideSchemes.UniversalChurchOfTruth (
  universalChurchOfTruth,
  UniversalChurchOfTruth (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Card.Code
import Marvel.Cost.Types
import Marvel.Entity
import Marvel.Game.Source (getBoostedPlayer)
import Marvel.GameValue
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Question (runAbility)
import Marvel.Ref
import Marvel.SideScheme.Cards qualified as Cards
import Marvel.SideScheme.Types
import Marvel.Window qualified as W

-- | 2 threat per player, an acceleration icon.
universalChurchOfTruth :: SideSchemeCard UniversalChurchOfTruth
universalChurchOfTruth = sideScheme UniversalChurchOfTruth Cards.universalChurchOfTruth (PerPlayer 2)

newtype UniversalChurchOfTruth = UniversalChurchOfTruth (Attrs SideScheme)
  deriving anyclass (HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Forced Response: After a player resets their deck, exhaust that player identity and
-- stun it: offered to the player who reset, before any optional response (Soul World).
instance IsSideScheme UniversalChurchOfTruth where
  sideSchemeAbilities (UniversalChurchOfTruth a) =
    [windowAbility a 1 (W.PlayerDeckResets W.After You) ForcedResponse NoCost $ runAbility a 1]

-- | [star] Boost: Reveal this card.
instance RunMessage UniversalChurchOfTruth where
  runMessage msg s@(UniversalChurchOfTruth attrs) = runQueueT $ case msg of
    RanAbility _ (isTarget attrs -> True) 1 [W.PlayerDeckReset p] _ -> do
      exhaust p
      stunIdentity p
      pure s
    Boost (RevealedAsBoost target enemy) | isTarget attrs target -> do
      revealBoost attrs =<< getBoostedPlayer enemy
      pure s
    _ -> UniversalChurchOfTruth <$> runMessage msg attrs
