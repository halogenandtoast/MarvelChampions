module Marvel.Upgrade.Upgrades.SpiderReflexesSilk where
import Marvel.Prelude
import Marvel.Effect.Types
import Marvel.Id
import Marvel.Cost
import Marvel.Criteria
import Marvel.Matchers
import Marvel.Question (runAbility)
import Marvel.Window qualified as W
import Marvel.Trait qualified as T ()
import Marvel.Damage ()
import Marvel.Game.Source
import Marvel.Card
import Marvel.Card.Id ()
import Marvel.Silk.Helpers

import Marvel.Ability
import Marvel.Card.Code ()
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Silk.Cards qualified as Cards
import Marvel.Upgrade.Types

spiderReflexesSilk :: UpgradeCard SpiderReflexesSilk
spiderReflexesSilk = upgrade SpiderReflexesSilk Cards.spiderReflexesSilk
newtype SpiderReflexesSilk = SpiderReflexesSilk (Attrs Upgrade)
  deriving anyclass (IsUpgrade, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)
instance RunMessage SpiderReflexesSilk where
  runMessage msg u@(SpiderReflexesSilk attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 windows _ -> do
      for_ [enemy | W.MadeBasicDefense player enemy <- windows, player == you] $ \enemy -> do
        cards <- matchingTuckedCards you (enemyRef enemy)
        addToIdentityBasicPower you (fromIntegral $ length cards)
        createEffect Cards.spiderReflexesSilk attrs (IdentityEntity $ IdentityWithId you)
      pure u
    _ -> SpiderReflexesSilk <$> runMessage msg attrs

instance HasAbilities SpiderReflexesSilk where
  getAbilities u@(SpiderReflexesSilk _attrs) = [limitedWindowAbility u 1 (W.MakesBasicDefense W.When You AnyEnemy) HeroInterrupt OwnsThis ExhaustCost (runAbility u 1)]

newtype SpiderReflexesSilkEffect = SpiderReflexesSilkEffect (Attrs Effect)
  deriving anyclass IsEffect
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsRef)

spiderReflexesSilkEffect :: CardEffect SpiderReflexesSilkEffect
spiderReflexesSilkEffect = effectWith SpiderReflexesSilkEffect Cards.spiderReflexesSilk (endsL ?~ DisableAtEndOfPhase)

instance RunMessage SpiderReflexesSilkEffect where
  runMessage msg effect@(SpiderReflexesSilkEffect attrs) = runQueueT $ case msg of
    IdentityMessage you (AttackDefended _ _ _) | effectMatcher attrs == IdentityEntity (IdentityWithId you) -> do
      cardSpecific attrs "tuckAfterDefense" you
      pure effect
    CardSpecific (isTarget attrs -> True) "tuckAfterDefense" (fromJSON -> Success (you :: IdentityId)) -> do
      cards <- getEncounterDiscard
      for_ (headMay cards) (tuckEncounterCard you)
      disableEffect (effectId attrs)
      pure effect
    _ -> SpiderReflexesSilkEffect <$> runMessage msg attrs
