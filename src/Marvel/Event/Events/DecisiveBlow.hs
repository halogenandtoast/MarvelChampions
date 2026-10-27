module Marvel.Event.Events.DecisiveBlow (
  decisiveBlow,
  DecisiveBlow (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Card.Def (CardMatcher (..), CardType (..), cardMatch)
import Marvel.Damage
import Marvel.Entity
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Game.Source
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Ref
import Marvel.Trait.Types qualified as Trait

decisiveBlow :: EventCard DecisiveBlow
decisiveBlow = event DecisiveBlow Cards.decisiveBlow

newtype DecisiveBlow = DecisiveBlow (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Deal 4 damage to an enemy, 7 if you have played a thwart event this turn.
instance RunMessage DecisiveBlow where
  runMessage msg e@(DecisiveBlow attrs) = runQueueT $ case msg of
    EventMessage ident msg' | ident == eventId attrs -> case msg' of
      PlayedEvent identityId _ _ -> do
        thwarted <-
          any (\(i, def) -> i == identityId && cardMatch (CardWithType EventType <> CardWithTrait Trait.Thwart) def)
            <$> getPlayedThisTurn
        enemies <- selectList AttackableEnemy
        chooseOne identityId
          [ targeting (enemyRef enemy) $
            dealDamage enemy attrs (toDamage (if thwarted then 7 else 4) $ FromPlayerAttack identityId)
          | enemy <- enemies
          ]
        pure e
      _ -> DecisiveBlow <$> runMessage msg attrs
    _ -> DecisiveBlow <$> runMessage msg attrs
