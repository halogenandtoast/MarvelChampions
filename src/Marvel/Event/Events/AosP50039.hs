{-# OPTIONS_GHC -Wno-unused-imports #-}
module Marvel.Event.Events.AosP50039 where

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

aosP50039 :: EventCard AosP50039
aosP50039 = event AosP50039 Cards.aosP50039

newtype AosP50039 = AosP50039 (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage AosP50039 where
  runMessage msg card@(AosP50039 attrs) = runQueueT $ case msg of
    EventMessage eid (PlayedEvent you _ _) | eid == eventId attrs -> do
      players <- getPlayers
      chooseOne you [targeting target do
        withTheVillain you $ \villain -> do
          minions <- selectList (MinionEngagedWith $ IdentityWithId target)
          let candidates = EnemyVillainId villain : map EnemyMinionId minions
          enemies <- filterM (selectAny . (AttackableBy (IdentityWithId you) <>) . EnemyWithId) candidates
          dealDamageToEach enemies attrs (withRanged $ toDamage 3 $ FromPlayerAttack you)
        | target <- players]
      pure card
    _ -> AosP50039 <$> runMessage msg attrs
