module Marvel.Event.Events.GoDownSwinging (goDownSwinging, GoDownSwinging (..)) where

import Marvel.Prelude
import Marvel.Card
import Marvel.Damage
import Marvel.Entity
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Game.Source
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Payment
import Marvel.Query
import Marvel.Ref

goDownSwinging :: EventCard GoDownSwinging
goDownSwinging = event GoDownSwinging Cards.goDownSwinging

newtype GoDownSwinging = GoDownSwinging (Attrs Event)
  deriving anyclass (HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance IsEvent GoDownSwinging where
  eventTargets (GoDownSwinging attrs) = do
    enemies <- selectList AnyEnemy
    valid <- filterM (\enemy -> not <$> cannotTakeDamageFrom (enemyRef enemy) (toSource attrs)) enemies
    pure $ if null valid then Just [] else Nothing

instance RunMessage GoDownSwinging where
  runMessage msg e@(GoDownSwinging attrs) = runQueueT $ case msg of
    EventMessage ident (PlayedEvent you payment _) | ident == eventId attrs -> do
      let amount = sum [fromIntegral $ max 0 $ fromMaybe 0 $ cdCost $ getCardDef card | card <- paymentDiscardedAllies payment]
      enemies <- selectList AnyEnemy
      valid <- filterM (\enemy -> not <$> cannotTakeDamageFrom (enemyRef enemy) (toSource attrs)) enemies
      unless (amount == 0 || null valid) $ chooseOne you
        [targeting (enemyRef enemy) (dealDamage enemy attrs $ toDamage amount FromAbility) | enemy <- valid]
      pure e
    _ -> GoDownSwinging <$> runMessage msg attrs
