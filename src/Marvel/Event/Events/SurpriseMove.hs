module Marvel.Event.Events.SurpriseMove where

import Marvel.Prelude
import Marvel.Ability ()
import Marvel.Card
import Marvel.Cost ()
import Marvel.Criteria ()
import Marvel.Damage
import Marvel.Entity
import Marvel.Game.Source ()
import Marvel.GameValue ()
import Marvel.Hand ()
import Marvel.Id
import Marvel.Matchers ()
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Payment ()
import Marvel.Projection ()
import Marvel.Identity.Types ()
import Marvel.Query ()
import Marvel.Question ()
import Marvel.Ref
import Marvel.Resource ()
import Marvel.Trait qualified as Trait ()
import Marvel.Window qualified as W
import Marvel.Iceman.Cards qualified as Cards
import Marvel.Iceman.Helpers ()
import Marvel.Event.Types

surpriseMove :: EventCard SurpriseMove
surpriseMove = event (SurpriseMove . (`With` Nothing)) Cards.surpriseMove

newtype SurpriseMove = SurpriseMove (Attrs Event `With` Maybe EnemyId)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance IsEvent SurpriseMove where
  toEventAttrs (SurpriseMove (attrs `With` _)) = attrs
instance HasModifiersFor SurpriseMove
instance RunMessage SurpriseMove where
  runMessage msg (SurpriseMove (attrs `With` target)) = runQueueT $ case msg of
    EventMessage eid (PlayedEvent you _ (Just (W.MadeBasicAttackAgainst _ enemy))) | eid == eventId attrs -> do
      addToBasicPower you 2
      pure $ SurpriseMove (attrs `With` Just enemy)
    CheckWindows ws | Just enemy <- target -> do
      let you = eventController attrs
          defeated = any (\case W.Window W.After (W.DefeatedMinion mid dmg _ _) -> enemy == EnemyMinionId mid && damageSource dmg == FromPlayerAttack you; W.Window W.After (W.DefeatedVillain vid dmg _ _) -> enemy == EnemyVillainId vid && damageSource dmg == FromPlayerAttack you; _ -> False) ws
          ended = W.Window W.After (W.MadeBasicAttack you) `elem` ws
      when defeated $ ready you
      updated <- runMessage msg attrs
      pure $ SurpriseMove (updated `With` if defeated || ended then Nothing else target)
    _ -> (\a -> SurpriseMove (a `With` target)) <$> runMessage msg attrs
