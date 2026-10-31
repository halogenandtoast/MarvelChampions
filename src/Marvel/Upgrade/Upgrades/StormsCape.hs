module Marvel.Upgrade.Upgrades.StormsCape where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Question
import Marvel.Cost
import Marvel.Criteria
import Marvel.Matchers hiding (ExhaustedIdentity)
import Marvel.Matchers qualified as Matchers
import Marvel.Window qualified as W
import Marvel.Trait
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Upgrade.Cards qualified as Cards
import Marvel.Upgrade.Types

stormsCape :: UpgradeCard StormsCape
stormsCape = upgrade StormsCape Cards.stormsCape

newtype StormsCape = StormsCape (Attrs Upgrade)
  deriving anyclass (IsUpgrade)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasModifiersFor StormsCape where
  getModifiersFor _ (IdentityRef you) (StormsCape attrs) | you == upgradeController attrs = do
    hero <- identityMatches HeroIdentity you
    pure [modifier | hero, modifier <- [DefenseModifier 1, TraitModifier Aerial]]
  getModifiersFor _ _ _ = pure []

instance HasAbilities StormsCape where
  getAbilities a = [limitedWindowAbility a 1 (W.WeatherSpecialResolvedFor W.After You) HeroResponse (OwnsThis <> SelfMatches Matchers.ExhaustedIdentity) ExhaustCost (runAbility a 1)]

instance RunMessage StormsCape where
  runMessage msg a@(StormsCape attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ _ -> a <$ ready you
    _ -> StormsCape <$> runMessage msg attrs
