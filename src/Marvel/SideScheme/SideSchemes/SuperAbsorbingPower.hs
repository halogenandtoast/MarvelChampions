module Marvel.SideScheme.SideSchemes.SuperAbsorbingPower (
  superAbsorbingPower,
  SuperAbsorbingPower (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Entity
import Marvel.Game.Source
import Marvel.GameValue
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.SideScheme.Cards qualified as Cards
import Marvel.SideScheme.Types
import Marvel.Trait

superAbsorbingPower :: SideSchemeCard SuperAbsorbingPower
superAbsorbingPower = sideScheme SuperAbsorbingPower Cards.superAbsorbingPower (PerPlayer 3)

newtype SuperAbsorbingPower = SuperAbsorbingPower (Attrs SideScheme)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance IsSideScheme SuperAbsorbingPower

-- | Absorbing Man, the villain, gains the Ice, Metal, Stone and Wood traits.
instance HasModifiersFor SuperAbsorbingPower where
  getModifiersFor _ (VillainRef _) _ = pure $ map TraitModifier [Ice, Metal, Stone, Wood]
  getModifiersFor _ _ _ = pure []

-- | [star] Boost: reveal this card.
instance RunMessage SuperAbsorbingPower where
  runMessage msg s@(SuperAbsorbingPower attrs) = runQueueT $ case msg of
    Boost (RevealedAsBoost target enemy) | isTarget attrs target -> do
      revealBoost attrs =<< getBoostedPlayer enemy
      pure s
    _ -> SuperAbsorbingPower <$> runMessage msg attrs
