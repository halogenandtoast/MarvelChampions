module Marvel.Event.Events.MakeTheCall
  ( makeTheCall
  , MakeTheCall(..)
  ) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Card.Def
import Marvel.Entity
import Marvel.Event.Attrs
import Marvel.Event.Cards qualified as Cards
import Marvel.Matchers
import Marvel.Message
import Marvel.Query
import Marvel.Question
import Marvel.Queue
import Marvel.Source
import Marvel.Target

makeTheCall :: EventCard MakeTheCall
makeTheCall = event MakeTheCall Cards.makeTheCall

newtype MakeTheCall = MakeTheCall EventAttrs
  deriving anyclass IsEvent
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, Entity, IsSource, IsTarget)

instance RunMessage MakeTheCall where
  runMessage msg e@(MakeTheCall attrs) = case msg of
    EventMessage eid msg' | eid == toId e -> case msg' of
      PlayedEvent identityId _ _ -> do
        discards <- selectList
          (AffordableCardBy (IdentityWithId identityId)
          $ InDiscardOf AnyIdentity
          $ BasicCardMatches (CardWithType AllyType)
          )
        pushAll
          [ FocusCards discards
          , Ask identityId
            $ ChooseOne [ PlayCard card Nothing | card <- discards ]
          , UnfocusCards
          ]
        pure e
      _ -> MakeTheCall <$> runMessage msg attrs
    _ -> MakeTheCall <$> runMessage msg attrs
