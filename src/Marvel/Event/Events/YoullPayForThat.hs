module Marvel.Event.Events.YoullPayForThat (
  youllPayForThat,
  YoullPayForThat (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Choice
import Marvel.Entity
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Identity.Types
import Marvel.Matchers
import Marvel.Message
import Marvel.Modifier
import Marvel.Projection
import Marvel.Question
import Marvel.Ref

youllPayForThat :: EventCard YoullPayForThat
youllPayForThat = event YoullPayForThat Cards.youllPayForThat

newtype YoullPayForThat = YoullPayForThat (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | After the villain attacks you: remove 1 threat from a scheme for each damage you took
-- from the attack, to a maximum of 5.
instance RunMessage YoullPayForThat where
  runMessage msg e@(YoullPayForThat attrs) = case msg of
    EventMessage ident msg' | ident == eventId attrs -> case msg' of
      PlayedEvent identityId _ _ -> do
        taken <- project PlayerIdentityAttackDamageTaken identityId
        when (taken > 0) $
          pushChoice identityId $ RemoveThreat (toSource attrs) (min 5 taken) ThwartableScheme
        pure e
      _ -> YoullPayForThat <$> runMessage msg attrs
    _ -> YoullPayForThat <$> runMessage msg attrs
