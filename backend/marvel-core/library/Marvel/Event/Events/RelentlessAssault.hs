module Marvel.Event.Events.RelentlessAssault
  ( relentlessAssault
  , RelentlessAssault(..)
  ) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Damage
import Marvel.Entity
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Matchers
import Marvel.Message
import Marvel.Modifier
import Marvel.Question
import Marvel.Resource
import Marvel.Source
import Marvel.Target

relentlessAssault :: EventCard RelentlessAssault
relentlessAssault = event RelentlessAssault Cards.relentlessAssault

newtype RelentlessAssault = RelentlessAssault (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsSource, IsTarget)

instance RunMessage RelentlessAssault where
  runMessage msg e@(RelentlessAssault attrs) = case msg of
    EventMessage ident msg' | ident == eventId attrs -> case msg' of
      PlayedEvent identityId payments _ -> do
        resources <- paymentResources payments
        let
          usedPhysical = Physical `elem` resources || Wild `elem` resources
          modifyDamage = if usedPhysical then withOverkill else id
        pushChoice identityId $ ChooseDamage
          (toSource attrs)
          (modifyDamage $ toDamage 5 $ FromPlayerAttack identityId)
          MinionEnemy
        pure e
      _ -> RelentlessAssault <$> runMessage msg attrs
    _ -> RelentlessAssault <$> runMessage msg attrs
