module Marvel.Event.Events.StrengthInNumbers (
  strengthInNumbers,
  StrengthInNumbers (..),
) where

import Marvel.Prelude

import Data.Aeson (Result (..), fromJSON)
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Matchers hiding (ExhaustedAlly)
import Marvel.Message
import Marvel.Modifier
import Marvel.Query
import Marvel.Question
import Marvel.Queue
import Marvel.Ref

strengthInNumbers :: EventCard StrengthInNumbers
strengthInNumbers = event StrengthInNumbers Cards.strengthInNumbers

newtype StrengthInNumbers = StrengthInNumbers (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | The player exhausts their allies one at a time, as many as they like, counting
-- them, then draws as many cards.
instance RunMessage StrengthInNumbers where
  runMessage msg e@(StrengthInNumbers attrs) = case msg of
    EventMessage ident (PlayedEvent _ _ _) | ident == eventId attrs -> do
      push $ cardSpecific attrs "exhaust" (0 :: Natural)
      pure e
    CardSpecific (isTarget attrs -> True) "exhaust" (fromJSON -> Success (n :: Natural)) -> do
      let identityId = eventController attrs
      allies <- selectList $ AllyMatches [AllyControlledBy (IdentityWithId identityId), UnexhaustedAlly]
      push $
        Ask identityId $
          ChooseOne $
            Label "$label.done" [Run [IdentityMessage identityId $ DrawCards FromDeck n | n > 0]]
              : [ TargetLabel (toRef a) [Run [AllyMessage a ExhaustedAlly, cardSpecific attrs "exhaust" (n + 1)]]
                | a <- allies
                ]
      pure e
    _ -> StrengthInNumbers <$> runMessage msg attrs
