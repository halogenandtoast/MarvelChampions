module Marvel.Event.Events.GoAllOut where

import Marvel.Prelude
import Marvel.Card.Code
import Marvel.Damage
import Marvel.Entity
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Hero.Runner qualified as Hero
import Marvel.Identity.Types (Field (PlayerIdentitySide, PlayerIdentitySides), PlayerIdentitySide (..))
import Marvel.Projection
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Ref

goAllOut :: EventCard GoAllOut
goAllOut = event GoAllOut Cards.goAllOut

newtype GoAllOut = GoAllOut (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage GoAllOut where
  runMessage msg a@(GoAllOut attrs) = runQueueT $ case msg of
    EventMessage ident (PlayedEvent you _ _) | ident == eventId attrs -> do
      side <- project PlayerIdentitySide you
      sides <- project PlayerIdentitySides you
      total <- case lookup side sides of
        Just (HeroSide hero) -> sum <$> sequence [Hero.getModifiedThwart (toAttrs hero), Hero.getModifiedAttack (toAttrs hero), Hero.getModifiedDefense (toAttrs hero)]
        _ -> pure 0
      enemies <- selectList AttackableEnemy
      chooseOne you [targeting (enemyRef enemy) (dealDamage enemy attrs $ toDamage total $ FromPlayerAttack you) | enemy <- enemies]
      pure a
    _ -> GoAllOut <$> runMessage msg attrs
