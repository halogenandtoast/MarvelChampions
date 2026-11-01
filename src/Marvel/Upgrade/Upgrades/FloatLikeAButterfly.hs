module Marvel.Upgrade.Upgrades.FloatLikeAButterfly where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Ability.Types qualified as Ability ()
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage ()
import Marvel.Entity
import Marvel.Game.Source ()
import Marvel.GameValue ()
import Marvel.Hand ()
import Marvel.Id ()
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Payment ()
import Marvel.Query ()
import Marvel.Question
import Marvel.Ref
import Marvel.Resource ()
import Marvel.Stats ()
import Marvel.Trait qualified as Trait ()
import Marvel.Window qualified as W
import Marvel.Upgrade.Cards qualified as Cards
import Marvel.Upgrade.Types
import Marvel.Attack (sourceEnemy)
import Marvel.Effect.Types

floatLikeAButterfly :: UpgradeCard FloatLikeAButterfly
floatLikeAButterfly = upgrade FloatLikeAButterfly Cards.floatLikeAButterfly

newtype FloatLikeAButterfly = FloatLikeAButterfly (Attrs Upgrade)
  deriving anyclass (IsUpgrade, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities FloatLikeAButterfly where
  getAbilities (FloatLikeAButterfly a) =
    [limitedWindowAbility a 1 (W.WindowOneOf
      [W.IdentityTargetsEnemy W.When (IdentityWithId $ upgradeController a) ConfusedEnemy
      ,W.AllyAttacked W.When (AllyControlledBy $ IdentityWithId $ upgradeController a) ConfusedEnemy])
      Interrupt OwnsThis NoCost (runAbility a 1)]

instance RunMessage FloatLikeAButterfly where
  runMessage msg e@(FloatLikeAButterfly a) = runQueueT $ case msg of
    RanAbility you (isTarget a -> True) 1 windows _ -> do
      for_ windows $ \case
        W.IdentityAttackTarget who enemy | who == you -> whenM (enemyMatches ConfusedEnemy enemy) $ do
          source <- currentPlayerAttackSource you
          for_ source $ \s -> do
            createEffect Cards.floatLikeAButterfly (enemyRef enemy) (attackSourceMatcher s)
            addToCurrentAttackAgainst you s enemy 1
        W.AllyAttack aid enemy -> whenM ((&&) <$> allyMatches (AllyControlledBy $ IdentityWithId you) aid <*> enemyMatches ConfusedEnemy enemy) $
          createEffect Cards.floatLikeAButterfly (enemyRef enemy) (AllyEntity $ AllyWithId aid)
        _ -> pure ()
      pure e
    _ -> FloatLikeAButterfly <$> runMessage msg a

newtype FloatLikeAButterflyEffect = FloatLikeAButterflyEffect (Attrs Effect)
  deriving anyclass IsEffect
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsRef)

floatLikeAButterflyEffect :: CardEffect FloatLikeAButterflyEffect
floatLikeAButterflyEffect = effectWith FloatLikeAButterflyEffect Cards.floatLikeAButterfly $ \a -> a
  {effectModifiers = [AttackDamageToEnemyModifier enemy 1 | Just enemy <- [sourceEnemy $ effectSource a]]}

instance RunMessage FloatLikeAButterflyEffect where
  runMessage msg e@(FloatLikeAButterflyEffect a) = runQueueT $ case msg of
    CheckWindows windows -> do
      whenM (anyM finished windows) $ disableEffect (effectId a)
      pure e
    EventMessage eid ResolvedEvent | effectMatcher a == SourceEntity (EventRef eid) -> e <$ disableEffect (effectId a)
    _ -> FloatLikeAButterflyEffect <$> runMessage msg a
   where
    finished (W.Window W.After window) = case window of
      W.PlayerAttackResolved source _ _ -> effectValidFor a (refocus source)
      W.MadeBasicAttack you -> effectValidFor a (IdentityRef you)
      W.AllyAttack aid _ -> effectValidFor a (AllyRef aid)
      W.AllyAttackCompletedAgainst aid _ -> effectValidFor a (AllyRef aid)
      _ -> pure False
    finished _ = pure False

attackSourceMatcher :: Source -> EntityMatcher
attackSourceMatcher = SourceEntity
