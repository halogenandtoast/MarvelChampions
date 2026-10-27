module Marvel.SideScheme.SideSchemes.SoundTheAlarms (
  soundTheAlarms,
  SoundTheAlarms (..),
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

soundTheAlarms :: SideSchemeCard SoundTheAlarms
soundTheAlarms = sideScheme SoundTheAlarms Cards.soundTheAlarms (PerPlayer 2)

newtype SoundTheAlarms = SoundTheAlarms (Attrs SideScheme)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance IsSideScheme SoundTheAlarms

-- | Each enemy gets +1 ATK.
instance HasModifiersFor SoundTheAlarms where
  getModifiersFor _ (VillainRef _) _ = pure [AttackModifier 1]
  getModifiersFor _ (MinionRef _) _ = pure [AttackModifier 1]
  getModifiersFor _ _ _ = pure []

-- | [star] Boost: reveal this card.
instance RunMessage SoundTheAlarms where
  runMessage msg s@(SoundTheAlarms attrs) = runQueueT $ case msg of
    Boost (RevealedAsBoost target enemy) | isTarget attrs target -> do
      revealBoost attrs =<< getBoostedPlayer enemy
      pure s
    _ -> SoundTheAlarms <$> runMessage msg attrs
