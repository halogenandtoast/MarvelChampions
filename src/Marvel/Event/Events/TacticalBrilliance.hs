module Marvel.Event.Events.TacticalBrilliance where

import Marvel.Prelude
import Marvel.Ability ()
import Marvel.Card
import Marvel.Cost ()
import Marvel.Criteria ()
import Marvel.Damage ()
import Marvel.Entity
import Marvel.Game.Source
import Marvel.GameValue ()
import Marvel.Hand ()
import Marvel.Id ()
import Marvel.Keyword ()
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Payment ()
import Marvel.Query
import Marvel.Question ()
import Marvel.Ref
import Marvel.Resource ()
import Marvel.Stats ()
import Marvel.Window qualified as W ()
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Trait qualified as Trait

tacticalBrilliance :: EventCard TacticalBrilliance
tacticalBrilliance = event TacticalBrilliance Cards.tacticalBrilliance

newtype TacticalBrilliance = TacticalBrilliance (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage TacticalBrilliance where
  runMessage msg e@(TacticalBrilliance attrs) = runQueueT $ case msg of
    EventMessage eid (PlayedEvent you _ _) | eid == eventId attrs -> do
      modifiers <- getModifiers you
      let permitted = ThwartableScheme <> SchemeWithoutThwartPrevention <>
            if CannotThwartSideSchemes `elem` modifiers then MainScheme else AnyScheme
      whenM (selectAny permitted) $ removeThreat you attrs 3 permitted
      cardSpecific attrs "recoverTactic" you
      pure e
    CardSpecific (isTarget attrs -> True) "recoverTactic" (fromJSON -> Success you) -> do
      cards <- selectList (InDiscardOf (IdentityWithId you) $ BasicCardMatches $ CardWithTrait Trait.Tactic)
      unless (null cards) $ chooseOne you [targeting card (addToHand you card) | card <- cards]
      pure e
    _ -> TacticalBrilliance <$> runMessage msg attrs
