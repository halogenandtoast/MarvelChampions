{-# OPTIONS_GHC -Wno-unused-imports #-}
module Marvel.Ally.Allies.AosP50091 where

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
import Marvel.Ally.Cards qualified as Cards
import Marvel.Ally.Runner

aosP50091 :: AllyCard AosP50091
aosP50091 = ally AosP50091 Cards.aosP50091 (Thw 1, 1) (Atk 1, 1) (HP 5)

newtype AosP50091 = AosP50091 (Attrs Ally)
  deriving anyclass (IsAlly)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities AosP50091 where
  getAbilities (AosP50091 attrs) =
    [ability attrs 1 HeroAction OwnsThis ExhaustCost (runAbility attrs 1)]

instance HasModifiersFor AosP50091 where
  getModifiersFor _ target (AosP50091 attrs) = pure $ [NotCountedAgainstAllyLimit | target == toRef attrs] <> [CannotLeavePlayByCardAbilities | target == toRef attrs]

instance RunMessage AosP50091 where
  runMessage msg card@(AosP50091 attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ _ -> do
      n <- fromIntegral . length <$> getPlayers
      removeThreat you attrs n MainScheme
      pure card
    _ -> AosP50091 <$> runMessage msg attrs
