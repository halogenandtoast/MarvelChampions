module Marvel.Event.Events.AllSystemsGo where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Deck
import Marvel.Discard
import Marvel.Identity.Types (Field (PlayerIdentityDeck, PlayerIdentityDiscard))
import Marvel.Projection
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Matchers
import Marvel.Query
import Marvel.Trait

allSystemsGo :: EventCard AllSystemsGo
allSystemsGo = event AllSystemsGo Cards.allSystemsGo

newtype AllSystemsGo = AllSystemsGo (Attrs Event)
  deriving anyclass (HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance IsEvent AllSystemsGo where
  eventTargets (AllSystemsGo attrs) = do
    let you = eventController attrs
    readyable <- selectAny (UpgradeWithTrait Interface <> upgradeControlledBy you <> UpgradeCanBeReadied)
    deck <- projectMap PlayerIdentityDeck unDeck you
    discard <- projectMap PlayerIdentityDiscard unDiscard you
    pure $ if readyable || not (null deck) || any (cardMatch $ CardWithTrait Interface) discard then Nothing else Just []

instance RunMessage AllSystemsGo where
  runMessage msg e@(AllSystemsGo attrs) = runQueueT $ case msg of
    EventMessage ident (PlayedEvent you _ _) | ident == eventId attrs -> do
      interfaces <- selectList (UpgradeWithTrait Interface <> upgradeControlledBy you <> UpgradeCanBeReadied)
      deck <- projectMap PlayerIdentityDeck unDeck you
      discard <- projectMap PlayerIdentityDiscard unDiscard you
      chooseOne you $
        [labeled "$label.spdrReadyInterfaces" (traverse_ ready interfaces) | notNull interfaces]
          <> [labeled "$label.spdrSearchInterface" $ search you (SearchIdentityDeckAndDiscardPile you) (CardWithTrait Interface) SearchDrawOne ShuffleBackIn | not (null deck) || any (cardMatch $ CardWithTrait Interface) discard]
      pure e
    _ -> AllSystemsGo <$> runMessage msg attrs
