{-# OPTIONS_GHC -Wno-unused-imports #-}
module Marvel.Support.Supports.AosP50040 where

import Marvel.Prelude
import Marvel.Question
import Marvel.Id
import Marvel.Projection
import Marvel.Card
import Marvel.Cost
import Marvel.Keyword
import Marvel.Criteria
import Marvel.Damage
import Marvel.Game.Source
import Marvel.Matchers
import Marvel.Payment
import Marvel.Query
import Marvel.Resource.Types qualified as R
import Marvel.Trait qualified as T
import Marvel.Window qualified as W
import Marvel.AgentsOfShield.PlayerHelpers

import Marvel.Ability
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Support.Cards qualified as Cards
import Marvel.Support.Types

aosP50040 :: SupportCard AosP50040
aosP50040 = support AosP50040 Cards.aosP50040

newtype AosP50040 = AosP50040 (Attrs Support)
  deriving anyclass (IsSupport, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities AosP50040 where
  getAbilities (AosP50040 attrs) =
    [ability attrs 1 HeroAction (OwnsThis) ((ExhaustCost <> CountersFromUpgradeCost (UpgradeWithKeyword SuitForm <> UpgradeControlledBy You) 1)) (runAbility attrs 1)]

instance RunMessage AosP50040 where
  runMessage msg card@(AosP50040 attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ _ -> do
      ready you
      lastingModifiers attrs (IdentityEntity $ IdentityWithId you) [TraitModifier T.Aerial] True
      pure card
    _ -> AosP50040 <$> runMessage msg attrs
