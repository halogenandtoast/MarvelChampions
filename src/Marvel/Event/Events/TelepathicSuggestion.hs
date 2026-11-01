module Marvel.Event.Events.TelepathicSuggestion where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Damage
import Marvel.Entity
import Marvel.Game.Source
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Ref
import Marvel.Upgrade.Cards qualified as Upgrades
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Window qualified as W

telepathicSuggestion :: EventCard TelepathicSuggestion
telepathicSuggestion = event TelepathicSuggestion Cards.telepathicSuggestion

newtype TelepathicSuggestion = TelepathicSuggestion (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage TelepathicSuggestion where
  runMessage msg a@(TelepathicSuggestion attrs) = runQueueT $ case msg of
    EventMessage eid (PlayedEvent you _ (Just (W.EncounterCardRevealed W.RevealedFromEncounterDeck _ card))) | eid == eventId attrs -> do
      cancelEncounterWhenRevealed card
      katanas <- selectListCount $ UpgradeControlledBy (IdentityWithId you) <> UpgradeIs Upgrades.psiKatana
      for_ [1..katanas] $ \_ -> cardSpecific attrs "damage" you
      cardSpecific attrs "knives" you
      pure a
    CardSpecific (isTarget attrs -> True) "damage" (fromJSON -> Success you) -> do
      whenM (selectAny AnyEnemy) $ chooseDamage you attrs (toDamage 2 FromAbility) AnyEnemy
      pure a
    CardSpecific (isTarget attrs -> True) "knives" (fromJSON -> Success you) -> do
      knives <- selectListCount $ UpgradeControlledBy (IdentityWithId you) <> UpgradeIs Upgrades.psiKnife
      for_ [1..knives] $ \_ -> cardSpecific attrs "thwart" you
      pure a
    CardSpecific (isTarget attrs -> True) "thwart" (fromJSON -> Success you) -> do
      whenM (selectAny ThwartableScheme) $ removeThreat you attrs 1 ThwartableScheme
      pure a
    _ -> TelepathicSuggestion <$> runMessage msg attrs
