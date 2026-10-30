module Marvel.Event.Events.ThwipThwip where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Card
import Marvel.Criteria
import Marvel.Matchers
import Marvel.Query
import Marvel.Damage
import Marvel.Keyword
import Marvel.Game.Source
import Marvel.Id (EnemyId)
import Marvel.Trait

thwipThwip :: EventCard ThwipThwip
thwipThwip = event ThwipThwip Cards.thwipThwip

newtype ThwipThwip = ThwipThwip (Attrs Event)
  deriving anyclass HasModifiersFor
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

canStun :: HasGame m => EnemyId -> m Bool
canStun enemy = do
  stunned <- enemyMatches StunnedEnemy enemy
  modifiers <- getModifiers (enemyRef enemy)
  keywords <- getsGame $ \g -> maybe [] (toList . cdKeywords) (abilityCardDef g $ enemyRef enemy)
  pure $ not stunned && Stalwart `notElem` keywords && KeywordModifier Stalwart `notElem` modifiers

instance IsEvent ThwipThwip where
  eventTargets _ = do
    possible <- anyM canStun =<< selectList AnyEnemy
    pure $ if possible then Nothing else Just []

instance RunMessage ThwipThwip where
  runMessage msg e@(ThwipThwip attrs) = runQueueT $ case msg of
    EventMessage ident (PlayedEvent you _ _) | ident == eventId attrs -> do
      cardSpecific attrs "placeStun" (you, 2 :: Natural)
      pure e
    CardSpecific (isTarget attrs -> True) "placeStun" (fromJSON -> Success (you, n :: Natural)) -> do
      enemies <- filterM canStun =<< selectList AnyEnemy
      when (n > 0 && notNull enemies) $ chooseOne you [targeting (enemyRef enemy) do
        stun enemy attrs
        cardSpecific attrs "placeStun" (you, n - 1) | enemy <- enemies]
      pure e
    _ -> ThwipThwip <$> runMessage msg attrs
