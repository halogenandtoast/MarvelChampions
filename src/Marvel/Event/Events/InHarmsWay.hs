module Marvel.Event.Events.InHarmsWay (
  inHarmsWay,
  InHarmsWay (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Damage
import Marvel.Entity
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Ref

inHarmsWay :: EventCard InHarmsWay
inHarmsWay = event InHarmsWay Cards.inHarmsWay

newtype InHarmsWay = InHarmsWay (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Hero Action (thwart): you take 2 damage (not from an attack), then 5 threat is
-- thwarted from a scheme.
instance RunMessage InHarmsWay where
  runMessage msg e@(InHarmsWay attrs) = runQueueT $ case msg of
    EventMessage ident (PlayedEvent you _ _) | ident == eventId attrs -> do
      damageIdentity you attrs (toDamage 2 FromAbility)
      cardSpecific attrs "thwart" ()
      pure e
    -- its own step: the schemes as they are once the damage is taken
    CardSpecific (isTarget attrs -> True) "thwart" _ -> do
      schemes <- selectList ThwartableScheme
      chooseOne (eventController attrs) [targeting s (thwartScheme s attrs 5) | s <- schemes]
      pure e
    _ -> InHarmsWay <$> runMessage msg attrs
