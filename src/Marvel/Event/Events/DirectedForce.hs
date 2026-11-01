module Marvel.Event.Events.DirectedForce where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Ability.Types qualified as Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage
import Marvel.Entity
import Marvel.Game.Source
import Marvel.GameValue
import Marvel.Hand
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Payment
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.Resource
import Marvel.Stats
import Marvel.Trait qualified as Trait
import Marvel.Window qualified as W
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Attack (sourceEnemy)
import Marvel.Effect.Types
import Marvel.Upgrade.Upgrades.FloatLikeAButterfly (attackSourceMatcher)

directedForce :: EventCard DirectedForce
directedForce = event DirectedForce Cards.directedForce

newtype DirectedForce = DirectedForce (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage DirectedForce where
  runMessage msg e@(DirectedForce a) = runQueueT $ case msg of
    EventMessage eid (PlayedEvent you _ (Just (W.MakingKeywordAttack _ source enemy _ targetSpecific))) | eid == eventId a -> do
      let matcher = attackSourceMatcher source
      createEffect Cards.directedForce (if targetSpecific then enemyRef enemy else GameRef) matcher
      addToKeywordAttackDamage you 2
      pure e
    EventMessage eid (PlayedEvent you _ (Just (W.MakingFriendlyKeywordAttack _ source _ _))) | eid == eventId a -> do
      createEffect Cards.directedForce GameRef (attackSourceMatcher source)
      addToKeywordAttackDamage you 2
      pure e
    _ -> DirectedForce <$> runMessage msg a

newtype DirectedForceEffect = DirectedForceEffect (Attrs Effect)
  deriving anyclass IsEffect
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsRef)

directedForceEffect :: CardEffect DirectedForceEffect
directedForceEffect = effectWith DirectedForceEffect Cards.directedForce $ \a -> a
  {effectModifiers = DirectedForceApplied : case sourceEnemy (effectSource a) of
    Just enemy -> [AttackDamageToEnemyModifier enemy 2]
    Nothing -> [DamageDealtModifier 2]}

instance RunMessage DirectedForceEffect where
  runMessage msg e@(DirectedForceEffect a) = runQueueT $ case msg of
    CheckWindows windows -> do
      whenM (anyM finished windows) $ disableEffect (effectId a)
      pure e
    EventMessage eid ResolvedEvent | effectMatcher a == SourceEntity (EventRef eid) -> e <$ disableEffect (effectId a)
    _ -> DirectedForceEffect <$> runMessage msg a
   where
    finished (W.Window W.After (W.PlayerAttackResolved source _ _)) = effectValidFor a (refocus source)
    finished (W.Window W.After (W.MadeBasicAttack you)) = effectValidFor a (IdentityRef you)
    finished _ = pure False
