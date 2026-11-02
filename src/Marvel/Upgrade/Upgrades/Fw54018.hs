{-# OPTIONS_GHC -Wno-unused-imports -Wno-unused-matches #-}
module Marvel.Upgrade.Upgrades.Fw54018 where
import Marvel.Prelude
import Data.Maybe (maybeToList)
import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage
import Marvel.Entity
import Marvel.Game.Source
import Marvel.GameValue
import Marvel.Hand
import Marvel.Hp
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Payment
import Marvel.Projection
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.Resource qualified as R
import Marvel.Stats
import Marvel.Trait qualified as T
import Marvel.Window qualified as W
import Marvel.FalconWinter.Cards qualified as Cards
import Marvel.Upgrade.Types

import Marvel.Keyword (Keyword(Restricted,Overkill))
import Marvel.Effect.Types
fw54018 :: UpgradeCard Fw54018
fw54018 = upgradeWith Fw54018 Cards.fw54018 ((usesL .~ 3) . (discardIfNoUsesL .~ True))

newtype Fw54018 = Fw54018 (Attrs Upgrade)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities Fw54018 where
  getAbilities a@(Fw54018 attrs) =
    [limitedWindowAbility a 1 (W.WindowOneOf $
      [W.MakesBasicAttack W.When $ IdentityWithId you | you <- maybeToList $ upgradeAttachedIdentity attrs]
      <> [W.AllyAttacked W.When (AllyWithId aid) AnyEnemy | aid <- maybeToList $ upgradeAttachedAlly attrs]) Interrupt OwnsThis (UseCost) (runAbility a 1)]

instance HasModifiersFor Fw54018 where
  getModifiersFor _ target (Fw54018 attrs)
    | target == toTarget attrs, isJust (upgradeAttachedIdentity attrs) = pure [KeywordModifier Restricted]
  getModifiersFor _ _ _ = pure []

instance IsUpgrade Fw54018 where
  upgradeTargets _ = Just . map characterRef <$> selectList (CharacterWithTrait T.Shield)

instance RunMessage Fw54018 where
  runMessage msg a@(Fw54018 attrs) = runQueueT $ case msg of
    UpgradeMessage uid (PlayedUpgrade (Just (IdentityRef who))) | uid == upgradeId attrs -> do
      upgradeStep uid (UpgradeAttachedToIdentity who)
      Fw54018 <$> runMessage msg attrs
    RanAbility you (isTarget attrs -> True) 1 windows payment -> do
      for_ (upgradeAttachedIdentity attrs) $ \who -> do
        addToBasicPower who 3
        createEffect Cards.fw54018 attrs (IdentityEntity $ IdentityWithId who)
      for_ (upgradeAttachedAlly attrs) $ \aid -> modifyAllyBasicAttack aid (withOverkill . (\d -> d {damageAmount = damageAmount d + 3}))
      pure a
    _ -> Fw54018 <$> runMessage msg attrs

newtype Fw54018Effect = Fw54018Effect (Attrs Effect)
  deriving anyclass IsEffect
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsRef)

fw54018Effect :: CardEffect Fw54018Effect
fw54018Effect = effectWith Fw54018Effect Cards.fw54018 (modifiersL .~ [BasicAttackKeyword Overkill])

instance RunMessage Fw54018Effect where
  runMessage msg e@(Fw54018Effect attrs) = runQueueT $ case msg of
    CheckWindows windows | any (\case W.Window W.After (W.MadeBasicAttack you) -> effectMatcher attrs == IdentityEntity (IdentityWithId you); _ -> False) windows -> e <$ disableEffect (effectId attrs)
    _ -> Fw54018Effect <$> runMessage msg attrs
