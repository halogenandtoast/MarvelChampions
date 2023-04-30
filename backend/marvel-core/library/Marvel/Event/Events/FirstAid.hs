module Marvel.Event.Events.FirstAid
  ( firstAid
  , FirstAid(..)
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
import Marvel.Query
import Marvel.Question
import Marvel.Source
import Marvel.Target

firstAid :: EventCard FirstAid
firstAid = event FirstAid Cards.firstAid

newtype FirstAid = FirstAid (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsSource, IsTarget)

instance RunMessage FirstAid where
  runMessage msg e@(FirstAid attrs) = case msg of
    EventMessage ident msg' | ident == eventId attrs -> case msg' of
      PlayedEvent identityId _ _ -> do
        characters <- selectList CharacterWithAnyDamage
        chooseOne identityId
          $ map (\c -> TargetLabel (CharacterTarget c) [Heal c 2]) characters
        pure e
      _ -> FirstAid <$> runMessage msg attrs
    _ -> FirstAid <$> runMessage msg attrs
