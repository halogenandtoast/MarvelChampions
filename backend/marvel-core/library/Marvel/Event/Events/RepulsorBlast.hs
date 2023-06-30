module Marvel.Event.Events.RepulsorBlast (
  repulsorBlast,
  RepulsorBlast (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Card.Def
import Marvel.Choice
import Marvel.Damage
import Marvel.Entity
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Matchers
import Marvel.Message
import Marvel.Modifier
import Marvel.Question
import Marvel.Queue
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
  runMessage msg e@(RepulsorBlast (attrs `With` meta)) = case msg of
    EventMessage ident msg' | ident == eventId attrs -> case msg' of
      PlayedEvent identityId _ _ -> do
        msgs <-
          choiceMessages identityId $
            ChooseEnemy AttackableEnemy (toTarget attrs)
        e <$ pushAll msgs
      _ -> RepulsorBlast . (`With` meta) <$> runMessage msg attrs
    ChoseEnemy enemy target | isTarget attrs target -> do
      push $
        IdentityMessage
          (eventController attrs)
          (DiscardFrom FromDeck 5 (Just $ toTarget attrs))
      pure . RepulsorBlast . (`With` Meta (Just $ toTarget enemy)) $ attrs
    WithDiscarded target _ cs | isTarget attrs target ->
      case blastTarget meta of
        Nothing -> error "invalid meta"
        Just enemy -> do
          let
            x =
              count (== Energy) $ concatMap (printedResources . getCardDef) cs
            ident = eventController attrs
          msgs <-
            choiceMessages ident $
              DamageEnemy
                enemy
                (toSource attrs)
                (toDamage (1 + (x * 2)) $ FromPlayerAttack ident)
          e <$ pushAll msgs
    _ -> RepulsorBlast . (`With` meta) <$> runMessage msg attrs
