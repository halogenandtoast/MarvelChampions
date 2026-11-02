{-# OPTIONS_GHC -Wno-unused-imports #-}
module Marvel.Ally.Allies.AosP50013 where

import Marvel.Prelude
import Marvel.Question
import Marvel.Id
import Marvel.Projection
import Marvel.Card
import Marvel.Effect.Types
import Marvel.Id
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

aosP50013 :: AllyCard AosP50013
aosP50013 = ally AosP50013 Cards.aosP50013 (Thw 2, 1) (Atk 2, 1) (HP 3)

newtype AosP50013 = AosP50013 (Attrs Ally)
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities AosP50013 where
  getAbilities (AosP50013 attrs) =
    [ ability ref 1 Action (ExtendedCardExists $ InHandOf You $ CardWithId $ CardId $ unAllyId $ allyId attrs) (ResourceCost $ Just R.Energy) (runAbility ref 1)
    , ability attrs 1 Action OwnsThis (ResourceCost $ Just R.Energy) (runAbility attrs 1)
    ]
   where ref = CardIdRef $ CardId $ unAllyId $ allyId attrs

instance RunMessage AosP50013 where
  runMessage msg card@(AosP50013 attrs) = runQueueT $ case msg of
    RanAbility _ (AllyRef aid) 1 _ _ | aid == allyId attrs -> do
      createEffect Cards.aosP50013 attrs (AllyEntity $ AllyWithId aid)
      pure card
    RanAbility you (CardIdRef cid) 1 _ _ | cid == CardId (unAllyId $ allyId attrs) -> do
      cards <- selectList (InHandOf (IdentityWithId you) $ BasicCardMatches $ CardIs Cards.aosP50013)
      players <- getPlayers
      for_ (take 1 cards) $ \played -> chooseOne you [targeting target do
        putIntoPlay target played
        createEffect Cards.aosP50013 played (AllyEntity $ AllyWithId $ AllyId $ unCardId $ pcCardId played)
        | target <- players]
      pure card
    _ -> AosP50013 <$> runMessage msg attrs

newtype SlingshotEffect = SlingshotEffect (Attrs Effect)
  deriving anyclass IsEffect
  deriving newtype (Show,Eq,ToJSON,FromJSON,IsRef)
slingshotEffect :: CardEffect SlingshotEffect
slingshotEffect = effect SlingshotEffect Cards.aosP50013
instance RunMessage SlingshotEffect where
  runMessage msg e@(SlingshotEffect attrs) = runQueueT $ case msg of
    EndPhase _ -> do
      case effectMatcher attrs of
        AllyEntity matcher -> selectList matcher >>= traverse_ returnToHand
        _ -> pure ()
      disableEffect (effectId attrs)
      pure e
    _ -> SlingshotEffect <$> runMessage msg attrs
