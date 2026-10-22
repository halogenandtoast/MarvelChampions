module Marvel.Event.Events.SevenRingsOfRaggadorr (
  sevenRingsOfRaggadorr,
  SevenRingsOfRaggadorr (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Entity
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Invocation
import Marvel.Matchers
import Marvel.Message
import Marvel.Modifier
import Marvel.Query
import Marvel.Question
import Marvel.Queue
import Marvel.Ref

sevenRingsOfRaggadorr :: EventCard SevenRingsOfRaggadorr
sevenRingsOfRaggadorr = event SevenRingsOfRaggadorr Cards.sevenRingsOfRaggadorr

newtype SevenRingsOfRaggadorr = SevenRingsOfRaggadorr (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Special: give up to 3 characters each a tough status card, one at a time, each one
-- without one already.
instance RunMessage SevenRingsOfRaggadorr where
  runMessage msg e@(SevenRingsOfRaggadorr attrs) = case msg of
    EventMessage ident msg' | ident == eventId attrs -> case msg' of
      PlayedEvent {} -> e <$ push (cardSpecific attrs "tough" (3 :: Natural))
      ResolvedEvent -> e <$ push (invocationResolved attrs)
    CardSpecific (isTarget attrs -> True) "tough" (fromJSON -> Success (n :: Natural)) | n > 0 -> do
      tough <- select (CharacterWithStatus ToughStatus)
      characters <- filter (not . (`member` tough)) <$> selectList AnyCharacter
      push $
        Ask (eventController attrs) $
          prompted "$label.chooseCharacterToGiveTough" $
            ChooseOne $
              Label "$label.done" []
                : [ TargetLabel (characterRef c) [Run [GiveStatus (characterRef c) ToughStatus, cardSpecific attrs "tough" (n - 1)]]
                  | c <- characters
                  ]
      pure e
    _ -> SevenRingsOfRaggadorr <$> runMessage msg attrs
