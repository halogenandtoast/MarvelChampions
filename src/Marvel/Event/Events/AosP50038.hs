{-# OPTIONS_GHC -Wno-unused-imports #-}
module Marvel.Event.Events.AosP50038 where

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
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Effect.Types

aosP50038 :: EventCard AosP50038
aosP50038 = event AosP50038 Cards.aosP50038

newtype AosP50038 = AosP50038 (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage AosP50038 where
  runMessage msg card@(AosP50038 attrs) = runQueueT $ case msg of
    EventMessage eid (PlayedEvent you _ _) | eid == eventId attrs -> do
      schemes <- selectList ThwartableScheme
      chooseOne you [targeting (schemeRef scheme) do
        createEffect Cards.aosP50038 attrs (IdentityEntity $ IdentityWithId you)
        thwartScheme scheme attrs 2
        cardSpecific attrs "covertSurveillance" you
        | scheme <- schemes]
      pure card
    CardSpecific (isTarget attrs -> True) "covertSurveillance" (fromJSON -> Success (you :: IdentityId)) -> do
      stealth <- isStealth you
      unless stealth $ chooseOne you [labeled "$label.done" (pure ()),labeled "$label.changeSuit" (changeSuit you True)]
      pure card
    _ -> AosP50038 <$> runMessage msg attrs

newtype CovertSurveillanceEffect = CovertSurveillanceEffect (Attrs Effect)
  deriving anyclass IsEffect
  deriving newtype (Show,Eq,ToJSON,FromJSON,IsRef)
covertSurveillanceEffect :: CardEffect CovertSurveillanceEffect
covertSurveillanceEffect = effect CovertSurveillanceEffect Cards.aosP50038
instance RunMessage CovertSurveillanceEffect where
  runMessage msg e@(CovertSurveillanceEffect attrs) = runQueueT $ case msg of
    CheckWindows windows -> do
      for_ [(scheme,n) | W.Window W.After (W.ThreatRemovedBy source scheme _ n) <- windows, source == effectSource attrs] $ \(scheme,n) -> do
        owner <- getSourceController (effectSource attrs)
        redirected <- elem RemovedThreatPlacedOnMinion <$> getModifiers (schemeRef scheme)
        for_ owner $ \you -> do
          stealth <- isStealth you
          when (stealth && not redirected && n > 0) $ chooseOne you
            [labeled "$label.done" (pure ()),labeled (countLabel "aosMoveThreat" n) (placeSuitThreat you n)]
        disableEffect (effectId attrs)
      when (any (\case W.Window W.After (W.EventAbilityResolved _ eid _) -> effectSource attrs == EventRef eid; _ -> False) windows) $ disableEffect (effectId attrs)
      pure e
    EndPhase _ -> e <$ disableEffect (effectId attrs)
    _ -> CovertSurveillanceEffect <$> runMessage msg attrs
