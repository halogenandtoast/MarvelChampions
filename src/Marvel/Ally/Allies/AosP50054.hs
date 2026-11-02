{-# OPTIONS_GHC -Wno-unused-imports #-}
module Marvel.Ally.Allies.AosP50054 where

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

aosP50054 :: AllyCard AosP50054
aosP50054 = ally AosP50054 Cards.aosP50054 (Thw 2, 1) (Atk 2, 1) (HP 3)

newtype AosP50054 = AosP50054 (Attrs Ally)
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities AosP50054 where
  getAbilities (AosP50054 attrs) =
    [limitedWindowAbility attrs 1 (W.AllyEntersPlay W.After $ AllyWithId $ allyId attrs) ForcedResponse OwnsThis NoCost (runAbility attrs 1)]

instance RunMessage AosP50054 where
  runMessage msg card@(AosP50054 attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ _ -> do
      chooseOne you
        [labeled "$label.removeThreat" (removeThreat you attrs 3 ThwartableScheme)
        ,labeled "$label.drawCards" (drawCards you 2)
        ,labeled "$label.giveTough" do
          characters <- selectList (CharacterWithTrait T.Shield)
          chooseOne you [targeting (characterRef character) (giveStatus character ToughStatus) | character <- characters]]
      pure card
    EndRound -> card <$ removeFromPlay attrs
    _ -> AosP50054 <$> runMessage msg attrs
