{-# OPTIONS_GHC -Wno-unused-imports #-}
module Marvel.Environment.Environments.AosP50032 where

import Marvel.Prelude
import Marvel.Question (runAbility)
import Marvel.Window qualified as W
import Marvel.Trait qualified as T
import Marvel.Query
import Marvel.Matchers
import Marvel.Game.Source
import Marvel.Cost
import Marvel.Ability
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Environment.Cards qualified as Cards
import Marvel.Environment.Types

aosP50032 :: EnvironmentCard AosP50032
aosP50032 = environment AosP50032 Cards.aosP50032

newtype AosP50032 = AosP50032 (Attrs Environment)
  deriving anyclass (IsEnvironment, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities AosP50032 where
  getAbilities (AosP50032 a) = [windowAbility a 1 (W.MinionDefeated W.After $ MinionWithTrait T.Controlled) ForcedResponse NoCost (runAbility a 1)]

instance RunMessage AosP50032 where
  runMessage msg e@(AosP50032 a) = runQueueT $ case msg of
    RanAbility _ (isTarget a -> True) 1 _ _ -> e <$ placeThreatOnEach a 1 MainScheme
    _ -> AosP50032 <$> runMessage msg a
