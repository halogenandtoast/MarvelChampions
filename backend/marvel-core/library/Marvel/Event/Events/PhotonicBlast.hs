module Marvel.Event.Events.PhotonicBlast
  ( photonicBlast
  , PhotonicBlast(..)
  ) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Damage
import Marvel.Entity
import Marvel.Event.Types
import Marvel.Event.Cards qualified as Cards
import Marvel.Matchers
import Marvel.Message
import Marvel.Modifier
import Marvel.Query
import Marvel.Question
import Marvel.Queue
import Marvel.Resource
import Marvel.Source
import Marvel.Target

photonicBlast :: EventCard PhotonicBlast
photonicBlast = event PhotonicBlast Cards.photonicBlast

newtype PhotonicBlast = PhotonicBlast EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, Entity, IsSource, IsTarget)

instance RunMessage PhotonicBlast where
  runMessage msg e@(PhotonicBlast attrs) = case msg of
    EventMessage eid msg' | eid == toId e -> case msg' of
      PlayedEvent identityId payments _ -> do
        resources <- paymentResources payments
        let usedEnergy = Energy `elem` resources || Wild `elem` resources
        when
          usedEnergy
          (push $ IdentityMessage identityId $ DrawCards FromDeck 1)
        enemies <- selectList AttackableEnemy
        chooseOne identityId $ map
          (damageChoice attrs (toDamage 5 $ FromPlayerAttack identityId))
          enemies
        pure e
      _ -> PhotonicBlast <$> runMessage msg attrs
    _ -> PhotonicBlast <$> runMessage msg attrs
