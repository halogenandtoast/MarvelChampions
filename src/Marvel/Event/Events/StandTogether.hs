module Marvel.Event.Events.StandTogether (standTogether, StandTogether (..)) where

import Marvel.Prelude
import Marvel.Attack (sourceEnemy)
import Marvel.Card.Code
import Marvel.Damage
import Marvel.Entity
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Id
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Window qualified as W

standTogether :: EventCard StandTogether
standTogether = event StandTogether Cards.standTogether

newtype StandTogether = StandTogether (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage StandTogether where
  runMessage msg e@(StandTogether attrs) = runQueueT $ case msg of
    EventMessage ident (PlayedEvent _ _ (Just window)) | ident == eventId attrs -> do
      let character = case window of
            W.IdentityTakeDamage hero _ -> Just (IdentityCharacter hero)
            W.AllyTakeDamage ally _ -> Just (AllyCharacter ally)
            _ -> Nothing
      for_ character \target -> do
        pending <- pendingCharacterAttackDamage target
        for_ pending \(source, damage) -> do
          cancelCharacterAttackDamage target
          for_ (sourceEnemy source) \enemy -> dealDamage enemy attrs (toDamage (damageAmount damage) FromAbility)
      pure e
    _ -> StandTogether <$> runMessage msg attrs
