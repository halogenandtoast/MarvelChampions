module Marvel.Event.Events.MakeTheCall (
  makeTheCall,
  MakeTheCall (..),
) where

import Marvel.Prelude

import Marvel.Game.Source
import Marvel.Identity (uniqueMatch)
import Marvel.Name
import Marvel.Payment
import Marvel.Card
import Marvel.Entity
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Ref

makeTheCall :: EventCard MakeTheCall
makeTheCall = event MakeTheCall Cards.makeTheCall

newtype MakeTheCall = MakeTheCall (Attrs Event)
  deriving anyclass (HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | The ally is chosen as the card is played, its printed cost paid with this card's: one
-- in any player's discard pile, not unique with a card in play.
instance IsEvent MakeTheCall where
  eventTargets _ = do
    allies <- selectList allyInDiscard
    inPlay <- getUniqueTitlesInPlay
    let
      clashes c =
        cdUnique (getCardDef c)
          && any (uniqueMatch (title $ cdName $ getCardDef c, maybe [] pure (subtitle $ cdName $ getCardDef c))) inPlay
    pure $ Just [toTarget c | c <- allies, not (clashes c)]

allyInDiscard :: ExtendedCardMatcher
allyInDiscard = InDiscardOf AnyIdentity $ BasicCardMatches (CardWithType AllyType)

-- | The ally is put into play under your control: it is not played.
instance RunMessage MakeTheCall where
  runMessage msg e@(MakeTheCall attrs) = runQueueT $ case msg of
    EventMessage ident (PlayedEvent identityId payment _) | ident == eventId attrs -> do
      allies <- selectList allyInDiscard
      for_ [c | Just (CardIdRef cid) <- [paymentTarget payment], c <- allies, pcCardId c == cid] $
        putIntoPlay identityId
      pure e
    _ -> MakeTheCall <$> runMessage msg attrs
