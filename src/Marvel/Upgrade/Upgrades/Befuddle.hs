module Marvel.Upgrade.Upgrades.Befuddle where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Entity
import Marvel.Game.Source ()
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.Upgrade.Cards qualified as Cards
import Marvel.Upgrade.Types
import Marvel.Window qualified as W
import Marvel.Effect.Types

befuddle :: UpgradeCard Befuddle
befuddle = upgrade Befuddle Cards.befuddle

newtype Befuddle = Befuddle (Attrs Upgrade)
  deriving anyclass (HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance IsUpgrade Befuddle where
  upgradeTargets _ = Just . map enemyRef <$> selectList
    (MinionEnemy <> NotEnemy (EnemyWithUpgrade $ UpgradeIs Cards.befuddle))

instance HasAbilities Befuddle where
  getAbilities a@(Befuddle attrs) =
    [ limitedWindowAbility attrs 1
        (W.WindowOneOf
          [ W.MakesBasicAttackAgainst W.When AnyIdentity (EnemyWithId enemy)
          , W.AllyAttacked W.When AnyAlly (EnemyWithId enemy)
          ]) Interrupt OwnsThis NoCost (runAbility a 1)
    | Just enemy <- [upgradeAttachedEnemy attrs]
    ]

instance RunMessage Befuddle where
  runMessage msg u@(Befuddle attrs) = runQueueT $ case msg of
    RanAbility _ (isTarget attrs -> True) 1 windows _ -> do
      for_ windows \case
        W.MadeBasicAttackAgainst ident _ -> createEffect Cards.befuddle attrs (IdentityEntity $ IdentityWithId ident)
        W.AllyAttack aid _ -> createEffect Cards.befuddle attrs (AllyEntity $ AllyWithId aid)
        _ -> pure ()
      pure u
    _ -> Befuddle <$> runMessage msg attrs

newtype BefuddleEffect = BefuddleEffect (Attrs Effect)
  deriving anyclass IsEffect
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsRef)

befuddleEffect :: CardEffect BefuddleEffect
befuddleEffect = effectWith BefuddleEffect Cards.befuddle (modifiersL .~ [AttackWithThwart])

instance RunMessage BefuddleEffect where
  runMessage msg e@(BefuddleEffect attrs) = runQueueT $ case msg of
    AllyMessage aid (AllyAttackLands enemy _) -> do
      active <- effectValidFor attrs (AllyRef aid)
      alive <- selectAny $ EnemyWithId enemy
      when (active && not alive) $ disableEffect (effectId attrs)
      pure e
    AllyMessage aid (AllyAttackMultipleFinished []) -> do
      whenM (effectValidFor attrs $ AllyRef aid) $ disableEffect (effectId attrs)
      pure e
    CheckWindows windows -> do
      done <- anyM finished windows
      when done $ disableEffect (effectId attrs)
      pure e
    _ -> BefuddleEffect <$> runMessage msg attrs
   where
    finished (W.Window W.After window) = case window of
      W.MadeBasicAttackAgainst ident _ -> effectValidFor attrs (IdentityRef ident)
      W.AllyAttack aid _ -> effectValidFor attrs (AllyRef aid)
      W.AllyAttackCompletedAgainst aid _ -> effectValidFor attrs (AllyRef aid)
      _ -> pure False
    finished _ = pure False
