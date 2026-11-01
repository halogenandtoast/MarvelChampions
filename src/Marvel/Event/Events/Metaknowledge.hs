module Marvel.Event.Events.Metaknowledge where

import Marvel.Prelude
import Marvel.Card
import Marvel.Damage
import Marvel.Entity
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Window qualified as W

metaknowledge :: EventCard Metaknowledge
metaknowledge = event Metaknowledge Cards.metaknowledge

newtype Metaknowledge = Metaknowledge (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage Metaknowledge where
  runMessage msg e@(Metaknowledge attrs) = runQueueT $ case msg of
    EventMessage eid (PlayedEvent you _ (Just (W.EncounterCardRevealed _ _ card))) | eid == eventId attrs -> do
      cancelEncounterCard card
      -- Every symbol in the printed boost area counts, including stars. Amplify
      -- adds icons only while a boost card is turned up, not during this reveal.
      let amount = fromIntegral $ length $ cdBoostIcons $ getCardDef card
      when (amount > 0) $ damageIdentity you attrs (toDamage amount FromAbility)
      pure e
    _ -> Metaknowledge <$> runMessage msg attrs
