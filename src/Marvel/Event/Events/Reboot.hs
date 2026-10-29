module Marvel.Event.Events.Reboot where

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
import Marvel.Payment
import Marvel.Upgrade.Cards qualified as Upgrades
import Marvel.Window qualified as W

reboot :: EventCard Reboot
reboot = event Reboot Cards.reboot

newtype Reboot = Reboot (Attrs Event)
  deriving anyclass (HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance IsEvent Reboot where
  eventTargets _ = do
    chars <- selectList (CharacterWithTrait Trait.Android <> CharacterControlledBy AnyIdentity)
    eligible <- filterM (\character -> (||)
      <$> characterMatches CharacterCanBeReadied character
      <*> characterMatches CharacterWithAnyDamage character) chars
    pure $ Just (map characterRef eligible)

instance RunMessage Reboot where
  runMessage msg e@(Reboot attrs) = runQueueT $ case msg of
    EventMessage eid (PlayedEvent _ payment _) | eid == eventId attrs -> do
      case paymentTarget payment of
        Just (IdentityRef ident) -> ready ident >> heal ident 1
        Just (AllyRef aid) -> ready aid >> heal aid 1
        _ -> pure ()
      pure e
    _ -> Reboot <$> runMessage msg attrs
