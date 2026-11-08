{-# OPTIONS_GHC -Wno-unused-imports #-}
module Marvel.Ally.Allies.AosP50023 where

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

aosP50023 :: AllyCard AosP50023
aosP50023 = ally AosP50023 Cards.aosP50023 (Thw 2, 1) (Atk 2, 1) (HP 3)

newtype AosP50023 = AosP50023 (Attrs Ally)
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities AosP50023 where
  getAbilities (AosP50023 attrs) =
    [limitedWindowAbility attrs index window Response OwnsThis NoCost (runAbility attrs 1)
    | (index,window) <- [(1,W.AllyAttacked W.After (AllyWithId $ allyId attrs) AnyEnemy),(2,W.AllyThwarted W.After (AllyWithId $ allyId attrs) AnyScheme),(3,W.AllyDefendsAgainst W.After (AllyWithId $ allyId attrs) AnyEnemy)]]

instance RunMessage AosP50023 where
  runMessage msg card@(AosP50023 attrs) = runQueueT $ case msg of
    RanAbility _ (isTarget attrs -> True) 1 _ _ -> do
      lookAtTopOfEncounterDeck 1 attrs
      pure card
    SearchFoundCards target (mapMaybe (\case EncounterCard c -> Just c; _ -> Nothing) -> cards) | isTarget attrs target -> do
      let you = allyController attrs
      lookingAt you (map EncounterCard cards) $
        chooseOne you [labeled "$label.done" (pure ()),labeled "$label.discard" (traverse_ discardCard cards)]
      pure card
    _ -> AosP50023 <$> runMessage msg attrs
