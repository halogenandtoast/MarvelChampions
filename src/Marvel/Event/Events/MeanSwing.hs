module Marvel.Event.Events.MeanSwing (
  meanSwing,
  MeanSwing (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Choice
import Marvel.Entity
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Matchers
import Marvel.Message
import Marvel.Modifier
import Marvel.Question
import Marvel.Queue
import Marvel.Ref
import Marvel.Trait

meanSwing :: EventCard MeanSwing
meanSwing = event MeanSwing Cards.meanSwing

newtype MeanSwing = MeanSwing (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | As the hero makes a basic attack: exhaust a Weapon upgrade on them → +3 ATK for it.
instance RunMessage MeanSwing where
  runMessage msg e@(MeanSwing attrs) = case msg of
    EventMessage ident msg' | ident == eventId attrs -> case msg' of
      PlayedEvent identityId _ _ -> do
        pushChoice identityId $
          ChooseUpgrade (UpgradeWithTrait Weapon <> UnexhaustedUpgrade <> upgradeControlledBy identityId) (toTarget attrs)
        pure e
      _ -> MeanSwing <$> runMessage msg attrs
    ChoseUpgrade weapon (isTarget attrs -> True) -> do
      push $ UpgradeMessage weapon ExhaustedUpgrade
      addToBasicPower (eventController attrs) 3
      pure e
    _ -> MeanSwing <$> runMessage msg attrs
