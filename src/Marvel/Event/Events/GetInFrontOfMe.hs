module Marvel.Event.Events.GetInFrontOfMe where

import Marvel.Prelude
import Marvel.Ability (HasAbilities)
import Marvel.Attack
import Marvel.Game.Source ()
import Marvel.Id (CharacterId (..))
import Marvel.Matchers ()
import Marvel.Query ()
import Marvel.Window qualified as W
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types

getInFrontOfMe :: EventCard GetInFrontOfMe
getInFrontOfMe = event GetInFrontOfMe Cards.getInFrontOfMe

newtype GetInFrontOfMe = GetInFrontOfMe (Attrs Event)
  deriving anyclass (IsEvent, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage GetInFrontOfMe where
  runMessage msg e@(GetInFrontOfMe attrs) = runQueueT $ case msg of
    EventMessage eid (PlayedEvent you _ (Just (W.RevealTreachery tid W.RevealedFromEncounterDeck))) | eid == eventId attrs -> do
      cancelWhenRevealed tid
      withTheVillain you $ \villain -> enemyAttacksWithCompletion villain you attrs
      pure e
    CardSpecific (isTarget attrs -> True) "attackCompleted" (fromJSON -> Success (result :: Maybe Attack)) -> do
      for_ result $ \resolved -> when (attackDefended resolved && defendedByOther (attackCharacter resolved)) $
        drawCards (eventController attrs) 1
      pure e
    _ -> GetInFrontOfMe <$> runMessage msg attrs
   where
    defendedByOther = \case
      AllyCharacter _ -> True
      IdentityCharacter you -> you /= eventController attrs
      _ -> False
