module Marvel.Event.Events.LethalIntent (lethalIntent, LethalIntent (..)) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Entity
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Hero.Heroes.NebulaHero (resolveTechnique)
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Payment
import Marvel.Query
import Marvel.Ref
import Marvel.Trait (Trait (Technique))

lethalIntent :: EventCard LethalIntent
lethalIntent = event LethalIntent Cards.lethalIntent

newtype LethalIntent = LethalIntent (Attrs Event)
  deriving anyclass (HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

controlledTechniques :: IdentityId -> UpgradeMatcher
controlledTechniques ident = UpgradeWithTrait Technique <> UpgradeControlledBy (IdentityWithId ident)

instance IsEvent LethalIntent where
  eventTargets (LethalIntent attrs) = do
    useful <- selectAny (controlledTechniques (eventController attrs) <> UpgradeWithResolvableSpecial)
    pure $ if useful then Nothing else Just []

instance RunMessage LethalIntent where
  runMessage msg e@(LethalIntent attrs) = runQueueT $ case msg of
    EventMessage eid (PlayedEvent _ payment _) | eid == eventId attrs -> do
      cardSpecific attrs "lethalIntentSelect" (paymentX payment, [] :: [UpgradeId])
      pure e
    CardSpecific (isTarget attrs -> True) "lethalIntentSelect" (fromJSON -> Success (remaining :: Natural, selected :: [UpgradeId])) -> do
      upgrades <- filter (`notElem` selected) <$> selectList (controlledTechniques ident)
      if remaining == 0 || null upgrades
        then cardSpecific attrs "lethalIntentResolve" selected
        else chooseOne ident $
          labeled "$label.done" (cardSpecific attrs "lethalIntentResolve" selected)
            : [targeting uid (cardSpecific attrs "lethalIntentSelect" (remaining - 1, selected <> [uid])) | uid <- upgrades]
      pure e
    CardSpecific (isTarget attrs -> True) "lethalIntentResolve" (fromJSON -> Success (selected :: [UpgradeId])) -> do
      -- Selection finishes before any Special resolves; resolution order is a
      -- separate choice and does not force the player to use selection order.
      unless (null selected) $ chooseOneAtATime ident
        [targeting uid (cardSpecific attrs "lethalIntentResolveOne" uid) | uid <- selected]
      pure e
    CardSpecific (isTarget attrs -> True) "lethalIntentResolveOne" (fromJSON -> Success uid) -> do
      resolveTechnique ident uid
      pure e
    _ -> LethalIntent <$> runMessage msg attrs
   where ident = eventController attrs
