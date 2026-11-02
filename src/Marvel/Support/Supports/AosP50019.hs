{-# OPTIONS_GHC -Wno-unused-imports #-}
module Marvel.Support.Supports.AosP50019 where

import Marvel.Prelude
import Marvel.Question
import Marvel.Id
import Marvel.Projection
import Marvel.Card
import Marvel.Cost
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

aosP50019 :: SupportCard AosP50019
aosP50019 = supportWith AosP50019 Cards.aosP50019 ((usesL .~ 3) . (discardIfNoUsesL .~ True))

newtype AosP50019 = AosP50019 (Attrs Support)
  deriving anyclass IsSupport
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities AosP50019 where
  getAbilities (AosP50019 attrs) =
    [ability attrs 1 Action (OwnsThis) (ExhaustCost <> UseCost) (runAbility attrs 1)]

instance RunMessage AosP50019 where
  runMessage msg card@(AosP50019 attrs) = runQueueT $ case msg of
    RanAbility _ (isTarget attrs -> True) 1 _ _ -> do
      schemes <- selectList AnyScheme
      for_ schemes $ \scheme -> thwartScheme scheme attrs 2
      pure card
    _ -> AosP50019 <$> runMessage msg attrs

instance HasModifiersFor AosP50019 where
  getModifiersFor _ target (AosP50019 attrs) = pure [IgnoresCrisis | target == toRef attrs]
