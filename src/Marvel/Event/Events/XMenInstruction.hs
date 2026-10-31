module Marvel.Event.Events.XMenInstruction (xMenInstruction, XMenInstruction (..)) where

import Marvel.Prelude
import Marvel.Card
import Marvel.Discard
import Marvel.Entity
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Identity.Types (Field (PlayerIdentityDiscard))
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Projection
import Marvel.Query
import Marvel.Ref
import Marvel.Trait qualified as Trait

xMenInstruction :: EventCard XMenInstruction
xMenInstruction = event XMenInstruction Cards.xMenInstruction

newtype XMenInstruction = XMenInstruction (Attrs Event)
  deriving anyclass HasModifiersFor
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance IsEvent XMenInstruction where
  eventTargets (XMenInstruction attrs) = do
    discards <- projectMap PlayerIdentityDiscard unDiscard (eventController attrs)
    mansion <- selectAny (SupportNamed "X-Mansion")
    pure $ if any eligible discards || mansion then Nothing else Just []

eligible :: PlayerCard -> Bool
eligible = cardMatch (CardWithType AllyType <> CardWithTrait Trait.XMen)

instance RunMessage XMenInstruction where
  runMessage msg a@(XMenInstruction attrs) = runQueueT $ case msg of
    EventMessage eid (PlayedEvent _ _ _) | eid == eventId attrs -> a <$ cardSpecific attrs "chooseCards" ([] :: [PlayerCard])
    CardSpecific (isTarget attrs -> True) "chooseCards" (fromJSON -> Success selected) -> do
      discards <- projectMap PlayerIdentityDiscard unDiscard you
      let candidates = filter (\card -> eligible card && card `notElem` selected) discards
      if length selected >= 2 || null candidates
        then cardSpecific attrs "finish" selected
        else chooseOne you $
          labeled "$label.done" (cardSpecific attrs "finish" selected)
            : [targeting card (cardSpecific attrs "chooseCards" (selected <> [card])) | card <- candidates]
      pure a
    CardSpecific (isTarget attrs -> True) "finish" (fromJSON -> Success (selected :: [PlayerCard])) -> do
      unless (null selected) $ shuffleIntoDeck you selected
      cardSpecific attrs "draw" ()
      pure a
    CardSpecific (isTarget attrs -> True) "draw" _ -> do
      whenM (selectAny $ SupportNamed "X-Mansion") $ drawCards you 1
      pure a
    _ -> XMenInstruction <$> runMessage msg attrs
   where
    you = eventController attrs
