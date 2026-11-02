{-# OPTIONS_GHC -Wno-unused-imports #-}
module Marvel.Ally.Allies.AosP50012 where

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

aosP50012 :: AllyCard AosP50012
aosP50012 = ally AosP50012 Cards.aosP50012 (Thw 2, 1) (Atk 1, 1) (HP 2)

newtype AosP50012 = AosP50012 (Attrs Ally)
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities AosP50012 where
  getAbilities (AosP50012 attrs) =
    [limitedWindowAbility attrs 1 (W.AllyEntersPlay W.After $ AllyWithId $ allyId attrs) Response OwnsThis NoCost (runAbility attrs 1)]

instance RunMessage AosP50012 where
  runMessage msg card@(AosP50012 attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ _ -> do
      supports <- selectList (SupportMatches [SupportWithTrait T.Shield,SupportWithExhaustion True])
      chooseOne you [targeting sid (ready sid) | sid <- supports]
      pure card
    _ -> AosP50012 <$> runMessage msg attrs
