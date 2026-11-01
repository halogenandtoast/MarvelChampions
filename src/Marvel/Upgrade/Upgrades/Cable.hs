module Marvel.Upgrade.Upgrades.Cable where

import Marvel.Prelude
import Data.List (delete)
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage
import Marvel.Status
import Marvel.Game.Source
import Marvel.Id
import Marvel.Matchers
import Marvel.Payment
import Marvel.Query
import Marvel.Resource
import Marvel.Trait qualified as Trait
import Marvel.Window qualified as W
import Marvel.Keyword qualified as Keyword
import {-# SOURCE #-} Marvel.Game (getPlayerVictoryDisplay, getVictoryDisplay, sideSchemesInVictoryDisplay)

import Marvel.Question (runAbility, Choice (Pay))
import Marvel.Ability
import Marvel.Modifier
import Marvel.Entity
import Marvel.Card
import Marvel.Ref
import Marvel.Message.Lifted
import Marvel.Upgrade.Cards qualified as Cards
import Marvel.Upgrade.Types
import Marvel.Stats
import Marvel.GameValue
import Marvel.Hand

cable40009 :: UpgradeCard Cable40009
cable40009 = upgrade Cable40009 Cards.cable40009

newtype Cable40009 = Cable40009 (Attrs Upgrade)
  deriving anyclass (IsUpgrade, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage Cable40009 where
  runMessage msg a@(Cable40009 attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ _ -> do
      n <- characterThwartValue (IdentityCharacter you)
      removeThreat you attrs n ThwartableScheme
      pure a
    _ -> Cable40009 <$> runMessage msg attrs

cable40010 :: UpgradeCard Cable40010
cable40010 = upgrade Cable40010 Cards.cable40010

newtype Cable40010 = Cable40010 (Attrs Upgrade)
  deriving anyclass (IsUpgrade, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage Cable40010 where
  runMessage msg a@(Cable40010 attrs) = runQueueT $ case msg of
    RanAbility _ (isTarget attrs -> True) 1 windows _ -> do
      for_ [sid | W.DefeatedSideSchemeBy sid _ _ <- windows] \sid -> do
        addDefeatedSideSchemeToVictoryDisplay sid
        addToVictoryDisplay attrs
      pure a
    _ -> Cable40010 <$> runMessage msg attrs

cable40011 :: UpgradeCard Cable40011
cable40011 = upgrade Cable40011 Cards.cable40011

newtype Cable40011 = Cable40011 (Attrs Upgrade)
  deriving anyclass (IsUpgrade, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage Cable40011 where
  runMessage msg a@(Cable40011 attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ _ -> do
      n <- min 4 <$> sideSchemesInVictoryDisplay
      chooseDamage you attrs (withRanged $ toDamage n $ FromPlayerAttack you) AttackableEnemy
      pure a
    _ -> Cable40011 <$> runMessage msg attrs

cable40012 :: UpgradeCard Cable40012
cable40012 = upgrade Cable40012 Cards.cable40012

newtype Cable40012 = Cable40012 (Attrs Upgrade)
  deriving anyclass (IsUpgrade, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage Cable40012 where
  runMessage msg a@(Cable40012 attrs) = runQueueT $ case msg of
    RanAbility _ (isTarget attrs -> True) 1 windows _ -> do
      for_ windows \case
        W.IdentityTakeDamage you dmg -> preventDamage you (damageAmount dmg)
        W.AllyTakeDamage aid dmg -> preventAllyDamage aid (damageAmount dmg)
        _ -> pure ()
      pure a
    _ -> Cable40012 <$> runMessage msg attrs

cable40013 :: UpgradeCard Cable40013
cable40013 = upgrade Cable40013 Cards.cable40013

newtype Cable40013 = Cable40013 (Attrs Upgrade)
  deriving anyclass (IsUpgrade, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage Cable40013 where
  runMessage msg a@(Cable40013 attrs) = runQueueT $ case msg of
    RanAbility _ (isTarget attrs -> True) 1 windows payment -> do
      for_ [main | W.MainSchemeCompleting main <- windows] \main ->
        for_ [sid | Just (SideSchemeRef sid) <- [paymentTarget payment]] \sid ->
          cardSpecific GameRef "cableTemporalLeapMove" (main, CardId $ unSideSchemeId sid)
      pure a
    _ -> Cable40013 <$> runMessage msg attrs

cable40023 :: UpgradeCard Cable40023
cable40023 = upgrade Cable40023 Cards.cable40023

newtype Cable40023 = Cable40023 (Attrs Upgrade)
  deriving anyclass (IsUpgrade, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage Cable40023 where
  runMessage msg a@(Cable40023 attrs) = runQueueT $ case msg of
    RanAbility _ (isTarget attrs -> True) 1 _ _ -> do
      players <- getPlayers
      for_ players (`drawCards` 1)
      pure a
    _ -> Cable40023 <$> runMessage msg attrs

cable40029 :: UpgradeCard Cable40029
cable40029 = upgrade Cable40029 Cards.cable40029

newtype Cable40029 = Cable40029 (Attrs Upgrade)
  deriving anyclass (IsUpgrade, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage Cable40029 where
  runMessage msg a@(Cable40029 attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ _ -> do
      chooseDamage you attrs (toDamage 2 $ FromPlayerAttack you) AttackableEnemy
      pure a
    _ -> Cable40029 <$> runMessage msg attrs

instance HasAbilities Cable40009 where
  getAbilities a@(Cable40009 attrs) =
    [limitedWindowAbility a 1 (W.HeroDefended W.After You AnyEnemy) HeroResponse OwnsThis
      (ExhaustCost <> ResourceCost (Just Energy)) (runAbility a 1)]

instance HasAbilities Cable40010 where
  getAbilities a@(Cable40010 attrs) =
    [limitedWindowAbility a 1 (W.SideSchemeDefeated W.After $ NotSideScheme $ SideSchemeWithKeyword Keyword.Permanent)
      HeroResponse OwnsThis NoCost (runAbility a 1)]

instance HasAbilities Cable40011 where
  getAbilities a@(Cable40011 attrs) =
    [ability a 1 HeroAction (OwnsThis <> VictoryDisplayHasSideScheme <> EnemyExists AttackableEnemy)
      (ExhaustCost <> ResourceCost (Just Energy)) (runAbility a 1)]

instance HasAbilities Cable40012 where
  getAbilities a@(Cable40012 attrs) =
    [limitedWindowAbility a 1 (W.WindowOneOf
      [ W.WouldTakeDamage AnyIdentity FromAnyDamageSource AnyValue
      , W.AllyWouldTakeDamage AnyAlly
      ]) HeroInterrupt OwnsThis (DiscardCost $ toRef attrs) (runAbility a 1)]

instance HasAbilities Cable40013 where
  getAbilities a@(Cable40013 attrs) =
    [limitedWindowAbility a 1 (W.MainSchemeWouldComplete MainScheme) HeroInterrupt OwnsThis
      ((RemoveFromGameCost $ toRef attrs) <> PutSideSchemeFromVictoryCost) (runAbility a 1)]

instance HasAbilities Cable40023 where
  getAbilities a@(Cable40023 attrs) =
    [limitedWindowAbility a 1 (W.SideSchemeDefeated W.After AnySideScheme) HeroResponse OwnsThis ExhaustCost (runAbility a 1)]

instance HasAbilities Cable40029 where
  getAbilities a@(Cable40029 attrs) =
    [limitedWindowAbility a 1 (W.PlaysCard W.After You $ CardWithTrait Trait.Psionic <> NotCardMatching (CardIs Cards.cable40029)) HeroResponse OwnsThis ExhaustCost (runAbility a 1)]
