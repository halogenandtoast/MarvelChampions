module Marvel.Event.Events.MakeTheCall (
  makeTheCall,
  MakeTheCall (..),
) where

import Marvel.Prelude

import Marvel.Card
import Marvel.Entity
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Matchers
import Marvel.Message
import Marvel.Modifier
import Marvel.Query
import Marvel.Question
import Marvel.Queue
import Marvel.Ref

makeTheCall :: EventCard MakeTheCall
makeTheCall = event MakeTheCall Cards.makeTheCall

newtype MakeTheCall = MakeTheCall (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage MakeTheCall where
  runMessage msg e@(MakeTheCall attrs) = case msg of
    EventMessage ident msg' | ident == eventId attrs -> case msg' of
      PlayedEvent identityId _ _ -> do
        discards <-
          selectList $
            AffordableCardBy (IdentityWithId identityId) $
              InDiscardOf AnyIdentity $
                BasicCardMatches (CardWithType AllyType)
        pushAll
          [ FocusCards $ PlayerCard <$> discards
          , Ask identityId $
              ChooseOne [PlayCard card Nothing | card <- discards]
          , UnfocusCards
          ]
        pure e
      _ -> MakeTheCall <$> runMessage msg attrs
    _ -> MakeTheCall <$> runMessage msg attrs
