module Marvel.Event.Events.PhoenixFirebird where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage
import Marvel.Entity
import Marvel.GameValue
import Marvel.Hand
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Payment
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.Resource
import Marvel.Stats
import Marvel.Trait qualified as Trait
import Marvel.Window qualified as W
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types

phoenixFirebird :: EventCard PhoenixFirebird
phoenixFirebird = event PhoenixFirebird Cards.phoenixFirebird

newtype PhoenixFirebird = PhoenixFirebird (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage PhoenixFirebird where
  runMessage msg e@(PhoenixFirebird a) = runQueueT $ case msg of
    EventMessage eid (PlayedEvent you _ _) | eid == eventId a -> do
      force <- selectAny (UpgradeNamed "Phoenix Force" <> upgradeControlledBy you <> UpgradeWithAnyUses)
      readyable <- characterMatches CharacterCanBeReadied (IdentityCharacter you)
      chooseOne you $ [labeled "$label.phoenixFirebirdReady" (removePhoenixPower you 1 >> ready you) | force && readyable]
        <> [labeled "$label.phoenixFirebirdPower" (addPhoenixPower you 2)]
      pure e
    _ -> PhoenixFirebird <$> runMessage msg a
