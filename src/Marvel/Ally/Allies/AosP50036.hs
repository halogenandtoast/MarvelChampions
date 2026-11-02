{-# OPTIONS_GHC -Wno-unused-imports #-}
module Marvel.Ally.Allies.AosP50036 where

import Marvel.Prelude
import Marvel.Question
import Marvel.Id
import Marvel.Projection
import Marvel.Card
import Marvel.Effect.Types
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

aosP50036 :: AllyCard AosP50036
aosP50036 = ally AosP50036 Cards.aosP50036 (Thw 2, 1) (Atk 1, 1) (HP 3)

newtype AosP50036 = AosP50036 (Attrs Ally)
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities AosP50036 where
  getAbilities (AosP50036 attrs) =
    [limitedWindowAbility attrs 1 (W.AllyThwarted W.When (AllyWithId $ allyId attrs) AnyScheme) Interrupt OwnsThis NoCost (runAbility attrs 1)]

instance RunMessage AosP50036 where
  runMessage msg card@(AosP50036 attrs) = runQueueT $ case msg of
    RanAbility _ (isTarget attrs -> True) 1 _ _ -> card <$ createEffect Cards.aosP50036 attrs (AllyEntity $ AllyWithId $ allyId attrs)
    _ -> AosP50036 <$> runMessage msg attrs

newtype MariaHillAllyEffect = MariaHillAllyEffect (Attrs Effect)
  deriving anyclass IsEffect
  deriving newtype (Show,Eq,ToJSON,FromJSON,IsRef)
mariaHillAllyEffect :: CardEffect MariaHillAllyEffect
mariaHillAllyEffect = effect MariaHillAllyEffect Cards.aosP50036
instance RunMessage MariaHillAllyEffect where
  runMessage msg e@(MariaHillAllyEffect attrs) = runQueueT $ case msg of
    CheckWindows windows -> do
      for_ [(aid,scheme,n) | W.Window W.After (W.ThreatRemovedBy (AllyRef aid) scheme _ n) <- windows,effectMatcher attrs == AllyEntity (AllyWithId aid)] $ \(aid,scheme,n) -> do
        owner <- getSourceController (AllyRef aid)
        redirected <- elem RemovedThreatPlacedOnMinion <$> getModifiers (schemeRef scheme)
        unless redirected $ traverse_ (\you -> placeSuitThreat you n) owner
        disableEffect (effectId attrs)
      when (any (\case W.Window W.After (W.AllyThwart aid _) -> effectMatcher attrs == AllyEntity (AllyWithId aid); _ -> False) windows) $ disableEffect (effectId attrs)
      pure e
    EndPhase _ -> e <$ disableEffect (effectId attrs)
    _ -> MariaHillAllyEffect <$> runMessage msg attrs
