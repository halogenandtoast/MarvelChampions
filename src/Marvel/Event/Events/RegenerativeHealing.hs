module Marvel.Event.Events.RegenerativeHealing where

import Marvel.Prelude
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Game.Source (getModifiers)
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Identity.Types (Field (..))
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Projection
import Marvel.Ref

regenerativeHealing :: EventCard RegenerativeHealing
regenerativeHealing = event RegenerativeHealing Cards.regenerativeHealing

newtype RegenerativeHealing = RegenerativeHealing (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage RegenerativeHealing where
  runMessage msg e@(RegenerativeHealing attrs) = runQueueT $ case msg of
    EventMessage eid (PlayedEvent you _ _) | eid == eventId attrs -> do
      damaged <- projectP PlayerIdentityDamage (> 0) you
      healable <- notElem CannotBeHealedByPlayerCards <$> getModifiers you
      stunned <- project PlayerIdentityStunned you
      confused <- project PlayerIdentityConfused you
      chooseOne you $
        [labeled (countLabel "healYourIdentity" 4) (heal you 4) | damaged && healable]
          <> [labeled "$label.regenerativeHealingStatuses" (removeStun you >> removeConfused you) | stunned || confused]
      pure e
    _ -> RegenerativeHealing <$> runMessage msg attrs
