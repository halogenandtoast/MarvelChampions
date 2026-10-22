module Marvel.Event.Events.GetOverHere (
  getOverHere,
  GetOverHere (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Choice
import Marvel.Damage
import Marvel.Entity
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Matchers
import Marvel.Message
import Marvel.Modifier
import Marvel.Query
import Marvel.Question
import Marvel.Queue
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
  runMessage msg e@(GetOverHere attrs) = case msg of
    EventMessage ident msg' | ident == eventId attrs -> case msg' of
      PlayedEvent identityId _ _ -> do
        pushChoice identityId $ ChooseEnemy MinionEnemy (toTarget attrs)
        pure e
      _ -> GetOverHere <$> runMessage msg attrs
    ChoseEnemy enemy@(EnemyMinionId minion) (isTarget attrs -> True) -> do
      let you = eventController attrs
      damage <- choiceMessages you $ DamageEnemy (toRef enemy) (toSource attrs) (toDamage 1 $ FromPlayerAttack you)
      pushAll $ damage <> [cardSpecific attrs "engage" minion]
      pure e
    CardSpecific (isTarget attrs -> True) "engage" (fromJSON -> Success minion) -> do
      let you = eventController attrs
      aerial <- selectAny $ IdentityWithId you <> IdentityWithTrait Aerial
      engageable <- selectAny $ MinionWithId minion <> NotMinion (MinionEngagedWith $ IdentityWithId you)
      when (aerial && engageable) $ pushAll $ engageMinion minion you
      pure e
    _ -> GetOverHere <$> runMessage msg attrs
