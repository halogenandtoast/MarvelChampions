module Marvel.Upgrade.Upgrades.SpiderClawsSilk where
import Marvel.Prelude
import Marvel.Effect.Types
import Marvel.Cost
import Marvel.Criteria
import Marvel.Matchers
import Marvel.Question (runAbility)
import Marvel.Window qualified as W
import Marvel.Trait qualified as T ()
import Marvel.Damage ()
import Marvel.Game.Source ()
import Marvel.Card
import Marvel.Card.Id ()
import Marvel.Silk.Helpers
import Marvel.Keyword

import Marvel.Ability
import Marvel.Card.Code ()
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Silk.Cards qualified as Cards
import Marvel.Upgrade.Types

spiderClawsSilk :: UpgradeCard SpiderClawsSilk
spiderClawsSilk = upgrade SpiderClawsSilk Cards.spiderClawsSilk
newtype SpiderClawsSilk = SpiderClawsSilk (Attrs Upgrade)
  deriving anyclass (IsUpgrade, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)
instance RunMessage SpiderClawsSilk where
  runMessage msg u@(SpiderClawsSilk attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 windows _ -> do
      for_ [enemy | W.MadeBasicAttackAgainst player enemy <- windows, player == you] $ \enemy -> do
        cards <- matchingTuckedCards you (enemyRef enemy)
        addToIdentityBasicPower you (fromIntegral $ length cards)
        createEffect Cards.spiderClawsSilk attrs (IdentityEntity $ IdentityWithId you)
      pure u
    _ -> SpiderClawsSilk <$> runMessage msg attrs

instance HasAbilities SpiderClawsSilk where
  getAbilities u@(SpiderClawsSilk _attrs) = [limitedWindowAbility u 1 (W.MakesBasicAttackAgainst W.When You AnyEnemy) HeroInterrupt OwnsThis ExhaustCost (runAbility u 1)]

newtype SpiderClawsSilkEffect = SpiderClawsSilkEffect (Attrs Effect)
  deriving anyclass (IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsRef)

spiderClawsSilkEffect :: CardEffect SpiderClawsSilkEffect
spiderClawsSilkEffect = effectWith SpiderClawsSilkEffect Cards.spiderClawsSilk
  ((modifiersL .~ [BasicAttackKeyword Piercing]) . (endsL ?~ DisableAtEndOfPhase))

instance RunMessage SpiderClawsSilkEffect where
  runMessage msg effect@(SpiderClawsSilkEffect attrs) = runQueueT $ case msg of
    CheckWindows windows | any finished windows -> effect <$ disableEffect (effectId attrs)
    _ -> SpiderClawsSilkEffect <$> runMessage msg attrs
   where
    finished (W.Window W.After (W.MadeBasicAttack you)) = effectMatcher attrs == IdentityEntity (IdentityWithId you)
    finished _ = False
