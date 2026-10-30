module Marvel.Event.Events.ShadowAndSteel where

import Marvel.Prelude
import Marvel.Card.Side
import Marvel.Payment
import Marvel.Game.Source
import Marvel.Hand
import Marvel.Stats
import Marvel.Status
import Marvel.Id
import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage
import Marvel.Entity
import Marvel.GameValue
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.Resource
import Marvel.Trait qualified as Trait
import Marvel.Window qualified as W
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types

shadowAndSteel :: EventCard ShadowAndSteel
shadowAndSteel = event ShadowAndSteel Cards.shadowAndSteel

newtype ShadowAndSteel = ShadowAndSteel (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage ShadowAndSteel where
  runMessage msg e@(ShadowAndSteel attrs) = runQueueT $ case msg of
    EventMessage eid (PlayedEvent you _ (Just (W.EnemyAttack enemy _))) | eid == eventId attrs -> do
      preventAttackDamage enemy
      dealDamage enemy attrs (toDamage 4 $ FromPlayerAttack you)
      pure e
    _ -> ShadowAndSteel <$> runMessage msg attrs
