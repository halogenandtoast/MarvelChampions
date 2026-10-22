module Marvel.Event.Events.RepulsorBlast (
  repulsorBlast,
  RepulsorBlast (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Card.Def
import Marvel.Damage
import Marvel.Entity
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Resource

repulsorBlast :: EventCard RepulsorBlast
repulsorBlast =
  event (RepulsorBlast . (`With` Meta Nothing)) Cards.repulsorBlast

newtype Meta = Meta {blastTarget :: Maybe Target}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

newtype RepulsorBlast = RepulsorBlast (Attrs Event `With` Meta)
  deriving anyclass (HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance IsEvent RepulsorBlast where
  toEventAttrs (RepulsorBlast (attrs `With` _)) = attrs

instance RunMessage RepulsorBlast where
  runMessage msg e@(RepulsorBlast (attrs `With` meta)) = runQueueT $ case msg of
    EventMessage ident msg' | ident == eventId attrs -> case msg' of
      PlayedEvent identityId _ _ -> do
        chooseEnemy identityId AttackableEnemy attrs
        pure e
      _ -> RepulsorBlast . (`With` meta) <$> runMessage msg attrs
    ChoseEnemy enemy target | isTarget attrs target -> do
      discardFromDeck (eventController attrs) 5 attrs
      pure . RepulsorBlast . (`With` Meta (Just $ toTarget enemy)) $ attrs
    WithDiscarded target _ cs | isTarget attrs target ->
      case blastTarget meta of
        Just (EnemyRef enemy) -> do
          let
            x = count (== Energy) $ concatMap (printedResources . getCardDef) cs
            ident = eventController attrs
          dealDamage enemy attrs (toDamage (1 + (x * 2)) $ FromPlayerAttack ident)
          pure e
        _ -> error "invalid meta"
    _ -> RepulsorBlast . (`With` meta) <$> runMessage msg attrs
