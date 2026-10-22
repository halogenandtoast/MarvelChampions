module Marvel.Event.Events.PreemptiveStrike (
  preemptiveStrike,
  PreemptiveStrike (..),
) where

import Marvel.Prelude

import Marvel.Boost
import Marvel.Card
import Marvel.Choice
import Marvel.Damage
import Marvel.Entity
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Id
import Marvel.Message
import Marvel.Modifier
import Marvel.Question
import Marvel.Queue
import Marvel.Ref
import Marvel.Window qualified as W

preemptiveStrike :: EventCard PreemptiveStrike
preemptiveStrike = event PreemptiveStrike Cards.preemptiveStrike

newtype PreemptiveStrike = PreemptiveStrike (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | The boost card's icons are added just after it turns face up: cancelling them takes
-- that off the queue.
instance RunMessage PreemptiveStrike where
  runMessage msg e@(PreemptiveStrike attrs) = case msg of
    EventMessage ident msg' | ident == eventId attrs -> case msg' of
      PlayedEvent identityId _ (Just (W.FlippedBoostCard card (EnemyVillainId vid))) -> do
        let cancelled = boostCount (cdBoostIcons $ getCardDef card)
        cancelMatchingMessage \case
          VillainMessage vid' (VillainAddBoost _) -> vid' == vid
          _ -> False
        when (cancelled > 0) $
          pushChoice identityId $
            DamageEnemy (toRef vid) (toSource attrs) (toDamage cancelled FromAbility)
        pure e
      _ -> PreemptiveStrike <$> runMessage msg attrs
    _ -> PreemptiveStrike <$> runMessage msg attrs
