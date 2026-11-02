{-# OPTIONS_GHC -Wno-unused-imports #-}
module Marvel.Event.Events.AosP50003 where

import Marvel.Prelude
import Marvel.Question
import Marvel.Id
import Marvel.Projection
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage
import Marvel.Game.Source
import Marvel.Matchers
import Marvel.Payment
import Marvel.Query
import Marvel.Resource.Types qualified as R
import Marvel.Trait qualified as T
import Marvel.Window qualified as W
import Marvel.AgentsOfShield.PlayerHelpers

import Marvel.Ability
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types

aosP50003 :: EventCard AosP50003
aosP50003 = event AosP50003 Cards.aosP50003

newtype AosP50003 = AosP50003 (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage AosP50003 where
  runMessage msg card@(AosP50003 attrs) = runQueueT $ case msg of
    EventMessage eid (PlayedEvent you _ _) | eid == eventId attrs -> do
      supports <- shieldSupports you
      for_ supports $ \_ -> chooseOne you
        [ labeled "$label.removeThreat" (removeThreat you attrs 1 ThwartableScheme)
        , labeled "$label.dealDamage" (chooseDamage you attrs (toDamage 1 FromAbility) DamageableEnemy)]
      pure card
    _ -> AosP50003 <$> runMessage msg attrs
