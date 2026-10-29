module Marvel.Event.Events.GodlikeStamina (godlikeStamina, GodlikeStamina (..)) where

import Marvel.Prelude
import Marvel.Card
import Marvel.Entity
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Ref
import Marvel.Status

godlikeStamina :: EventCard GodlikeStamina
godlikeStamina = event GodlikeStamina Cards.godlikeStamina

newtype GodlikeStamina = GodlikeStamina (Attrs Event)
  deriving anyclass (HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance IsEvent GodlikeStamina where
  eventTargets (GodlikeStamina attrs) = do
    let you = eventController attrs
    damaged <- identityMatches IdentityWithAnyDamage you
    statuses <- selectList $ IdentityCharacters (IdentityWithId you) <> CharacterWithStatus StunnedStatus
    confused <- identityMatches ConfusedIdentity you
    tough <- not <$> identityMatches IdentityWithoutToughStatus you
    pure $ if damaged || not (null statuses) || confused || tough then Nothing else Just []

instance RunMessage GodlikeStamina where
  runMessage msg e@(GodlikeStamina attrs) = runQueueT $ case msg of
    EventMessage eid (PlayedEvent you _ _) | eid == eventId attrs -> do
      heal you 2
      cardSpecific attrs "discardStatus" ()
      pure e
    CardSpecific (isTarget attrs -> True) "discardStatus" _ -> do
      let you = eventController attrs
      stunned <- identityMatches StunnedIdentity you
      confused <- identityMatches ConfusedIdentity you
      tough <- not <$> identityMatches IdentityWithoutToughStatus you
      when (stunned || confused || tough) $ chooseOne you $
        [labeled (statusLabel StunnedStatus) (removeStatus you StunnedStatus) | stunned]
        <> [labeled (statusLabel ConfusedStatus) (removeStatus you ConfusedStatus) | confused]
        <> [labeled (statusLabel ToughStatus) (removeStatus you ToughStatus) | tough]
        <> [labeled "$label.done" (pure ())]
      pure e
    _ -> GodlikeStamina <$> runMessage msg attrs
