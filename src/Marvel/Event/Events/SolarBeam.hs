module Marvel.Event.Events.SolarBeam where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Ability qualified as Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage
import Marvel.Entity
import Marvel.Game.Source
import Marvel.Query
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Question
import Marvel.Ref
import Marvel.Trait qualified as Trait
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Upgrade.Cards qualified as Upgrades
import Marvel.Window qualified as W

solarBeam :: EventCard SolarBeam
solarBeam = event SolarBeam Cards.solarBeam

newtype SolarBeam = SolarBeam (Attrs Event)
  deriving anyclass (HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance IsEvent SolarBeam where
  eventAbilitySubTypes (SolarBeam a) = do
    dense <- isInMassForm (eventController a) Upgrades.dense
    pure [if dense then Ability.Attack else Ability.Thwart]

instance RunMessage SolarBeam where
  runMessage msg e@(SolarBeam attrs) = runQueueT $ case msg of
    EventMessage eid (PlayedEvent you _ _) | eid == eventId attrs -> do
      dense <- isInMassForm you Upgrades.dense
      if dense
        then chooseDamage you attrs (toDamage 7 $ FromPlayerAttack you) AttackableEnemy
        else removeThreat you attrs 5 ThwartableScheme
      pure e
    _ -> SolarBeam <$> runMessage msg attrs
