module Marvel.Treachery.Treacheries.TemporalTrickery where

import Marvel.Prelude
import Marvel.Card
import Marvel.Hand
import Marvel.Identity.Types (Field (PlayerIdentityHand))
import Marvel.Matchers
import Marvel.Projection
import Marvel.Question ()
import Marvel.Ability
import Marvel.Card.Code ()
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Treachery.Cards qualified as Cards
import Marvel.Treachery.Types

temporalTrickery :: TreacheryCard TemporalTrickery
temporalTrickery = treachery TemporalTrickery Cards.temporalTrickery

newtype TemporalTrickery = TemporalTrickery (Attrs Treachery)
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)



instance RunMessage TemporalTrickery where
  runMessage msg a@(TemporalTrickery attrs) = runQueueT $ case msg of
    TreacheryMessage tid (RevealTreachery you) | tid == treacheryId attrs -> do
      hand <- projectMap PlayerIdentityHand unHand you
      let count = length . printedResources . getCardDef
          most = maximum (0 : map count hand)
      chooseOne you [targeting card $ do
        discardCard card
        placeThreatOnEach attrs (fromIntegral most) AnyScheme
        | card <- hand, count card == most]
      pure a
    _ -> TemporalTrickery <$> runMessage msg attrs
