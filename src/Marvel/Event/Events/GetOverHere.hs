module Marvel.Event.Events.GetOverHere (
  getOverHere,
  GetOverHere (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Damage
import Marvel.Entity
import Marvel.Id
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Ref
import Marvel.Trait

getOverHere :: EventCard GetOverHere
getOverHere = event GetOverHere Cards.getOverHere

newtype GetOverHere = GetOverHere (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | 1 damage to a minion, then, if you are Aerial, it engages you: unless the damage
-- defeated it, or it is engaged with you already.
instance RunMessage GetOverHere where
  runMessage msg e@(GetOverHere attrs) = runQueueT $ case msg of
    EventMessage ident msg' | ident == eventId attrs -> case msg' of
      PlayedEvent identityId _ _ -> do
        chooseEnemy identityId MinionEnemy attrs
        pure e
      _ -> GetOverHere <$> runMessage msg attrs
    ChoseEnemy enemy@(EnemyMinionId minion) (isTarget attrs -> True) -> do
      let you = eventController attrs
      dealDamage enemy attrs (toDamage 1 $ FromPlayerAttack you)
      cardSpecific attrs "engage" minion
      pure e
    CardSpecific (isTarget attrs -> True) "engage" (fromJSON -> Success minion) -> do
      let you = eventController attrs
      aerial <- selectAny $ IdentityWithId you <> IdentityWithTrait Aerial
      engageable <- selectAny $ MinionWithId minion <> NotMinion (MinionEngagedWith $ IdentityWithId you)
      when (aerial && engageable) $ engageMinion minion you
      pure e
    _ -> GetOverHere <$> runMessage msg attrs
