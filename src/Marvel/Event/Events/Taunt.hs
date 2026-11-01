module Marvel.Event.Events.Taunt where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Entity
import Marvel.Game.Source
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.Window qualified as W
import Marvel.Effect.Types
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types

taunt :: EventCard Taunt
taunt = event Taunt Cards.taunt
newtype Taunt = Taunt (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage Taunt where
  runMessage msg e@(Taunt attrs) = runQueueT $ case msg of
    EventMessage eid (PlayedEvent you _ _) | eid == eventId attrs -> do
      withTheVillain you $ \villain -> do
        createEffect Cards.taunt attrs (EnemyEntity $ EnemyWithId $ EnemyVillainId villain)
        cardSpecific attrs "configureTaunt" you
        enemyAttacks villain you
      cardSpecific attrs "draw" you
      pure e
    CardSpecific (isTarget attrs -> True) "draw" (fromJSON -> Success (you :: IdentityId)) -> e <$ drawCards you 3
    _ -> Taunt <$> runMessage msg attrs

newtype TauntEffect = TauntEffect (Attrs Effect)
  deriving anyclass IsEffect
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsRef)

tauntEffect :: CardEffect TauntEffect
tauntEffect = effect TauntEffect Cards.taunt

instance RunMessage TauntEffect where
  runMessage msg e@(TauntEffect attrs) = runQueueT $ case msg of
    CardSpecific source "configureTaunt" (fromJSON -> Success (you :: IdentityId)) | toSource source == effectSource attrs ->
      pure $ TauntEffect attrs {effectModifiers = [OnlyIdentityCanDefend you]}
    CheckWindows windows | any ended windows -> e <$ disableEffect (effectId attrs)
    CardSpecific source "draw" _ | toSource source == effectSource attrs -> e <$ disableEffect (effectId attrs)
    _ -> TauntEffect <$> runMessage msg attrs
    where
      ended (W.Window W.After (W.EnemyAttack enemy _)) = effectMatcher attrs == EnemyEntity (EnemyWithId enemy)
      ended (W.Window W.After (W.EnemyAttackAlly enemy _ _)) = effectMatcher attrs == EnemyEntity (EnemyWithId enemy)
      ended _ = False
