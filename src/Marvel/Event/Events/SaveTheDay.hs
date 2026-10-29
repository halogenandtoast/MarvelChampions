module Marvel.Event.Events.SaveTheDay (saveTheDay, SaveTheDay (..)) where

import Marvel.Prelude
import Marvel.Card
import Marvel.Damage
import Marvel.Entity
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Game.Source
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Payment
import Marvel.Query
import Marvel.Ref

saveTheDay :: EventCard SaveTheDay
saveTheDay = event SaveTheDay Cards.saveTheDay

newtype SaveTheDay = SaveTheDay (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage SaveTheDay where
  runMessage msg e@(SaveTheDay attrs) = runQueueT $ case msg of
    EventMessage ident (PlayedEvent you payment _) | ident == eventId attrs -> do
      let amount = sum [fromIntegral $ max 0 $ fromMaybe 0 $ cdCost $ getCardDef card | card <- paymentDiscardedAllies payment]
      schemes <- selectList ThwartableScheme
      unless (amount == 0 || null schemes) $ chooseOne you
        [targeting (schemeRef scheme) (thwartScheme scheme attrs amount) | scheme <- schemes]
      pure e
    _ -> SaveTheDay <$> runMessage msg attrs
