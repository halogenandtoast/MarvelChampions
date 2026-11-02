{-# OPTIONS_GHC -Wno-unused-imports -Wno-unused-matches #-}
module Marvel.Upgrade.Upgrades.Fw54011 where
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

import Marvel.Keyword (Keyword(Piercing,Ranged))
import Marvel.Effect.Types
import Marvel.Keyword (Keyword(Piercing,Ranged))
fw54011 :: UpgradeCard Fw54011
fw54011 = upgrade Fw54011 Cards.fw54011

newtype Fw54011 = Fw54011 (Attrs Upgrade)
  deriving anyclass (IsUpgrade, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities Fw54011 where
  getAbilities a@(Fw54011 attrs) =
    [limitedWindowAbility a 1 (W.MakesBasicAttack W.When $ IdentityWithId $ upgradeController attrs) HeroInterrupt (OwnsThis) (ExhaustCost) (runAbility a 1)]

instance RunMessage Fw54011 where
  runMessage msg a@(Fw54011 attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 windows payment -> do
      addToBasicPower you 2
      createEffect Cards.fw54011 attrs (IdentityEntity $ IdentityWithId you)
      pure a
    _ -> Fw54011 <$> runMessage msg attrs

newtype Fw54011Effect = Fw54011Effect (Attrs Effect)
  deriving anyclass IsEffect
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsRef)

fw54011Effect :: CardEffect Fw54011Effect
fw54011Effect = effectWith Fw54011Effect Cards.fw54011 (modifiersL .~ [BasicAttackKeyword Piercing, BasicAttackKeyword Ranged])

instance RunMessage Fw54011Effect where
  runMessage msg e@(Fw54011Effect attrs) = runQueueT $ case msg of
    CheckWindows windows | any (\case W.Window W.After (W.MadeBasicAttack you) -> effectMatcher attrs == IdentityEntity (IdentityWithId you); _ -> False) windows -> e <$ disableEffect (effectId attrs)
    _ -> Fw54011Effect <$> runMessage msg attrs
